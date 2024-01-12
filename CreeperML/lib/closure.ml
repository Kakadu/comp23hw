(** Copyright 2023-2024, Arthur Alekseev and Starcev Matvey *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

module ClosureAst = struct
  open Indexed_ast.IndexedTypeAst
  open Parser_ast.ParserAst
  open Type_ast.TypeAst

  type cf_expr =
    | CFApply of cf_typ_expr * cf_typ_expr list
    | CFLiteral of literal
    | CFValue of string
    | CFTuple of cf_typ_expr list
    | CFIfElse of cf_if_else
    | CFClosure of string * (string, ty) typed list

  and cf_if_else = {
    cond : cf_typ_expr;
    t_body : cf_typ_expr;
    f_body : cf_typ_expr;
  }

  and cf_typ_expr = (cf_expr, ty) typed

  type cf_typ_let_binding = {
    rec_f : rec_flag;
    l_v : index_lvalue;
    cf_body : cf_typ_let_body;
  }

  and cf_typ_let_body = {
    cf_lets : cf_typ_let_binding list;
    cf_expr : cf_typ_expr;
  }

  type cf_fun_let_binding = {
    is_rec : rec_flag;
    name : (string, ty) typed;
    args : index_lvalue list;
    b : cf_typ_let_body;
    env_vars : (string, ty) typed list;
  }

  type cf_binding =
    | FunBinding of cf_fun_let_binding
    | ValBinding of cf_typ_let_binding

  type cf_typ_program = cf_binding list
end

module ClosureConvert = struct
  open ClosureAst
  open Type_ast.TypeAst
  open Parser_ast.ParserAst
  open Indexed_ast.IndexedTypeAst
  open State
  open Std

  module TypedName = struct
    type t = (string, ty) typed

    let compare (n1 : t) (n2 : t) = compare n1.value n2.value
  end

  module NameSet = Set.Make (TypedName)

  let typed t e : ('a, ty) typed = { value = e; typ = t }

  let rec fst_typ_name_of_lvalue (lval : (ilvalue, ty) typed) =
    match (lval.value, lval.typ) with
    | DLvTuple xs, TyTuple typs ->
        List.hd xs |> typed (List.hd typs) |> fst_typ_name_of_lvalue
    | DLvValue v, ty -> typed ty v
    | _, ty -> typed ty "_"

  let rec typ_names_of_lvalue (lval : (ilvalue, ty) typed) =
    match (lval.value, lval.typ) with
    | DLvTuple vs, TyTuple typs ->
        let e = NameSet.empty in
        let valtyps = List.map2 (fun x y -> (x, y)) vs typs in
        List.fold_left
          (fun xs x ->
            NameSet.union xs @@ typ_names_of_lvalue (typed (snd x) @@ fst x))
          e valtyps
    | DLvValue v, ty -> typed ty v |> NameSet.singleton
    | _, ty -> typed ty "_" |> NameSet.singleton

  let rec collect_unbound_variables (f : index_fun_body) global_bindings =
    let rec collect_variables_in_expr (e : index_expr) =
      match e.value with
      | DApply (left, right) ->
          let left = collect_variables_in_expr left in
          let right = collect_variables_in_expr right in
          NameSet.union left right
      | DLiteral _ -> NameSet.empty
      | DValue name -> NameSet.singleton @@ typed e.typ name
      | DTuple exprs ->
          List.fold_left NameSet.union NameSet.empty
            (List.map collect_variables_in_expr exprs)
      | DIfElse ite ->
          let i = collect_variables_in_expr ite.cond in
          let t = collect_variables_in_expr ite.t_body in
          let e = collect_variables_in_expr ite.f_body in
          NameSet.union i t |> NameSet.union e
      | DFun f -> collect_unbound_variables f global_bindings
    in

    let rec collect_variables_in_let (l : index_let_binding) known =
      let e = NameSet.empty in
      let u = NameSet.union in
      let d = NameSet.diff in
      let inner (kn, unk) x =
        let nkn, nukn = collect_variables_in_let x kn in
        (u nkn kn, u nukn unk)
      in
      match l.rec_f with
      | Rec ->
          let known = u known @@ typ_names_of_lvalue l.l_v in
          let iknown, iunknown = List.fold_left inner (known, e) l.body.lets in
          let expr_unknowns = collect_variables_in_expr l.body.expr in
          let expr_unknowns = d expr_unknowns iknown in
          (known, u iunknown expr_unknowns)
      | NoRec ->
          let iknown, iunknown = List.fold_left inner (known, e) l.body.lets in
          let known = u known @@ typ_names_of_lvalue l.l_v in
          let expr_unknowns = collect_variables_in_expr l.body.expr in
          let expr_unknowns = d expr_unknowns iknown in
          (known, u iunknown expr_unknowns)
    in

    let collect_variables_in_body (b : index_let_body) known =
      let e = NameSet.empty in
      let u = NameSet.union in
      let known, unknown =
        List.fold_left
          (fun (kn, unk) x ->
            let new_kn, new_unk = collect_variables_in_let x kn in
            (u kn new_kn, u unk new_unk))
          (known, e) b.lets
      in
      let unknown = NameSet.union unknown @@ collect_variables_in_expr b.expr in
      (known, unknown)
    in

    let known = NameSet.union global_bindings @@ typ_names_of_lvalue f.lvalue in
    let known, unknown = collect_variables_in_body f.b known in
    NameSet.diff unknown known

  let remove_redefinitons p =
    let rec filter_vb (vb : cf_typ_let_binding) =
      match (vb.l_v.value, vb.cf_body.cf_expr.value, vb.cf_body.cf_lets) with
      | DLvValue v, CFValue n, [] when v = n -> None
      | _ ->
          let new_lets = List.filter_map filter_vb vb.cf_body.cf_lets in
          Some { vb with cf_body = { vb.cf_body with cf_lets = new_lets } }
    in
    let filter_fb (fb : cf_fun_let_binding) =
      let new_lets = List.filter_map filter_vb fb.b.cf_lets in
      { fb with b = { fb.b with cf_lets = new_lets } }
    in
    let filter_prog = function
      | ValBinding vb -> filter_vb vb |> Option.map (fun x -> ValBinding x)
      | FunBinding fb -> Some (FunBinding (filter_fb fb))
    in
    List.filter_map filter_prog p

  let rec closure_free_expr globals r cn (e : index_expr) =
    let* (new_cn : string) = new_var "l" in
    let cf_expr = closure_free_expr globals r new_cn in
    match e.value with
    | DApply (left, right) ->
        let* left_decs, left = cf_expr left in
        let* right_decs, right = cf_expr right in
        let result = CFApply (left, [ right ]) |> typed e.typ in
        return (left_decs @ right_decs, result)
    | DLiteral literal -> return ([], CFLiteral literal |> typed e.typ)
    | DValue name -> return ([], CFValue name |> typed e.typ)
    | DTuple exprs ->
        let* bindings, cf_exprs = monadic_map exprs cf_expr >>| List.split in
        return (bindings |> List.concat, CFTuple cf_exprs |> typed e.typ)
    | DIfElse ite ->
        let* i_decs, i_expr = cf_expr ite.cond in
        let* t_decs, t_expr = cf_expr ite.t_body in
        let* e_decs, e_expr = cf_expr ite.f_body in
        let res =
          CFIfElse { cond = i_expr; t_body = t_expr; f_body = e_expr }
        in
        let res_typed = res |> typed e.typ in
        return (i_decs @ t_decs @ e_decs, res_typed)
    | DFun f ->
        let rec convert_fun f args =
          match (f.b.lets, f.b.expr.value) with
          | [], DFun f -> convert_fun f (args @ [ f.lvalue ])
          | _, _ ->
              let bound =
                args
                |> List.map typ_names_of_lvalue
                |> List.fold_left NameSet.union globals
              in
              let unknown_vars = collect_unbound_variables f bound in
              let* inner = monadic_map f.b.lets (cf_let globals r) in
              let* expr_closures, cf_expr = cf_expr f.b.expr in
              let inner_closures, inner_cf_lets = List.split inner in
              let inner_closures = List.concat inner_closures in
              let cf_body = { cf_lets = inner_cf_lets; cf_expr } in
              let env = NameSet.to_seq unknown_vars |> List.of_seq in
              let env_lval =
                List.map (fun v -> DLvValue v.value |> typed v.typ) env
              in
              let fun_let =
                {
                  is_rec = r;
                  name = typed e.typ cn;
                  args;
                  b = cf_body;
                  env_vars = env;
                }
              in
              let typ_cf_val = CFValue cn |> typed e.typ in
              let f =
                match env_lval with
                | [] -> typ_cf_val
                | _ -> CFClosure (cn, env) |> typed e.typ
              in
              return (inner_closures @ expr_closures @ [ fun_let ], f)
        in
        convert_fun f [ f.lvalue ]

  and cf_let (globals : NameSet.t) rec_f (l : index_let_binding) :
      (cf_fun_let_binding list * cf_typ_let_binding) t =
    let is_rec =
      match (l.rec_f, rec_f) with NoRec, NoRec -> NoRec | _ -> Rec
    in
    let to_bool = function Rec -> true | _ -> false in
    let globals =
      if to_bool is_rec then NameSet.union globals (typ_names_of_lvalue l.l_v)
      else globals
    in
    let let_name = fst_typ_name_of_lvalue l.l_v in
    let* closures, cf_expr =
      closure_free_expr globals is_rec ("l" ^ let_name.value) l.body.expr
    in
    let* _, inner_closures, rev_binds =
      monadic_fold
        (fun (g, fns, bnds) x ->
          let* funs, bind = cf_let g is_rec x in
          let is_fun (b : cf_typ_let_binding) =
            match
              List.find_opt (fun x -> DLvValue x.name.value = b.l_v.value) funs
            with
            | Some _ -> true
            | _ -> false
          in
          return
            ( (if is_fun bind then NameSet.union g (typ_names_of_lvalue bind.l_v)
               else g),
              fns @ funs,
              bind :: bnds ))
        (globals, [], []) l.body.lets
    in
    let inner_cf_lets = rev_binds |> List.rev in
    let cf_let_body : cf_typ_let_body = { cf_lets = inner_cf_lets; cf_expr } in
    let rec_f = l.rec_f in
    let l_v = l.l_v in
    let cf_let_binding = { rec_f; l_v; cf_body = cf_let_body } in
    return (inner_closures @ closures, cf_let_binding)

  and cf_of_index prog =
    let rec inner g (p : index_program) acc =
      match p with
      | h :: t ->
          let* closures, binding = cf_let g h.rec_f h in
          let closures = List.map (fun x -> FunBinding x) closures in
          let binding = ValBinding binding in
          let u = NameSet.union in
          let g = typ_names_of_lvalue (h.l_v.value |> typed h.l_v.typ) |> u g in
          (closures, binding) :: acc |> inner g t
      | [] -> return acc
    in
    let set = Std.operators |> List.to_seq |> NameSet.of_seq in
    inner set prog [] |> run |> List.split |> fun (closures, bindings) ->
    List.concat closures @ List.rev bindings |> remove_redefinitons
end
