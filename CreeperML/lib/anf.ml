(** Copyright 2023-2024, Arthur Alekseev and Starcev Matvey *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

module AnfTypeAst = struct
  open Type_ast.TypeAst
  open Parser_ast.ParserAst
  open Indexed_ast.IndexedTypeAst

  type tlvalue = index_lvalue
  type tliteral = (literal, ty) typed
  type tname = (string, ty) typed
  type imm = ImmVal of tname | ImmLit of tliteral

  type anf_expr =
    | AApply of imm * imm list
    | ATuple of imm list
    | Aite of imm * anf_body * anf_body
    | AImm of imm
    | ATupleAccess of imm * int
    | AClosure of tname * imm list

  and anf_body = { lets : anf_val_binding list; res : imm }
  and anf_val_binding = { name : tname; e : anf_expr }

  type anf_fun_binding = {
    name : tname;
    args : tname list;
    env : tname list;
    body : anf_body;
  }

  type anf_binding = AnfVal of anf_val_binding | AnfFun of anf_fun_binding
  type anf_program = anf_binding list
end

module AnfConvert = struct
  open AnfTypeAst
  open Type_ast.TypeAst
  open Closure.ClosureAst
  open State

  (* constructors *)
  let app l r = AApply (l, r)
  let tup l = ATuple l
  let aite i t e = Aite (i, t, e)
  let aval x = AnfVal x
  let imm i = AImm i
  let imml l = ImmLit l
  let immv v = ImmVal v
  let tname t name : tname = { typ = t; value = name }
  let tliteral t literal : tliteral = { typ = t; value = literal }
  let binding name expr : anf_val_binding = { name; e = expr }
  let body lets res : anf_body = { lets; res }
  let tlvalue typ value : tlvalue = { value; typ }

  let rec anf_of_expr (e : cf_typ_expr) : (anf_val_binding list * imm) t =
    match e.value with
    | CFApply (fn, args) ->
        let rec apply_inner fn args =
          match fn.value with
          | CFApply (fn, old_args) -> apply_inner fn (old_args @ args)
          | _ ->
              let* fn_bindings, fn_imm = anf_of_expr fn in
              let* arg_bindings, arg_imms =
                monadic_map args anf_of_expr >>| List.split
              in
              let arg_bindings = List.concat arg_bindings in
              let* self_tname = new_var "i" >>| tname e.typ in
              let self_binding = app fn_imm arg_imms |> binding self_tname in
              return
                ( (self_binding :: arg_bindings) @ fn_bindings,
                  self_tname |> immv )
        in
        apply_inner fn args
    | CFIfElse ite ->
        let* i_bindings, if_imm = anf_of_expr ite.cond in
        let* t_bindings, then_imm = anf_of_expr ite.t_body in
        let* e_bindings, else_imm = anf_of_expr ite.f_body in
        let t_body = body (List.rev t_bindings) then_imm in
        let e_body = body (List.rev e_bindings) else_imm in
        let* self_tname = new_var "i" >>| tname e.typ in
        let self_binding = aite if_imm t_body e_body |> binding self_tname in
        return (self_binding :: i_bindings, self_tname |> immv)
    | CFLiteral l -> return ([], l |> tliteral e.typ |> imml)
    | CFValue v -> return ([], v |> tname e.typ |> immv)
    | CFTuple els ->
        let* bindings, tuple_imms =
          monadic_map els anf_of_expr >>| List.split
        in
        let bindings = bindings |> List.concat in
        let* self_tname = new_var "i" >>| tname e.typ in
        let self_binding = tup tuple_imms |> binding self_tname in
        return (self_binding :: bindings, self_tname |> immv)
    | CFClosure (c, env) ->
        let env = List.map immv env in
        let* self_tname = new_var "i" >>| tname e.typ in
        let self_binding =
          AClosure (c |> tname e.typ, env) |> binding self_tname
        in
        return ([ self_binding ], self_tname |> immv)

  let rec lv_binds (lv : tlvalue) (er : imm) : anf_val_binding list t =
    match (lv.value, lv.typ) with
    | DLvValue name, _ -> return [ binding (tname lv.typ name) (imm er) ]
    | DLvTuple lvalues, TyTuple typs ->
        let inner index (elem, etyp) =
          let* t_name = new_var "i" >>| tname etyp in
          let access = ATupleAccess (er, index) |> binding t_name in
          let telem = tlvalue etyp elem in
          let* lvbnds = lv_binds telem (t_name |> immv) in
          access :: lvbnds |> return
        in
        let zipped = List.map2 (fun x y -> (x, y)) lvalues typs in
        let* decs = monadic_mapi zipped inner in
        List.concat decs |> return
    | DLvUnit, _ ->
        let* new_name = new_var "i" in
        return [ binding (tname lv.typ new_name) (imm er) ]
    | _, _ -> return []

  let rec anf_of_let_binding (l : cf_typ_let_binding) : anf_val_binding list t =
    let reversed_lets = l.cf_body.cf_lets |> List.rev in
    let* bindings = monadic_map reversed_lets anf_of_let_binding in
    let bindings = List.concat bindings in
    let* expr_bindings, expr_res = anf_of_expr l.cf_body.cf_expr in
    let* lv_res = lv_binds l.l_v expr_res in
    List.rev lv_res @ expr_bindings @ bindings |> return

  let anf_of_fun_binding (l : cf_fun_let_binding) : anf_fun_binding t =
    let reversed_lets = l.b.cf_lets |> List.rev in
    let* bindings = monadic_map reversed_lets anf_of_let_binding in
    let bindings = List.concat bindings in
    let* expr_bindings, res = anf_of_expr l.b.cf_expr in
    let inner2 a =
      let* arg_name = new_var "i" >>| tname a.typ in
      let arg_imm = arg_name |> immv in
      let* arg_decs = lv_binds a arg_imm in
      return (arg_decs, arg_name)
    in
    let* arg_decs, arg_names = monadic_map l.args inner2 >>| List.split in
    let arg_decs = List.concat arg_decs in
    let lets = expr_bindings @ bindings @ arg_decs |> List.rev in
    let body = { lets; res } in
    let name = l.name in
    return { name; args = arg_names; env = l.env_vars; body }

  let anf_of_cf (p : cf_typ_program) : anf_program =
    let is_val_binding = function AnfFun _ -> false | AnfVal _ -> true in
    let inner = function
      | FunBinding fb ->
          let* binding = anf_of_fun_binding fb in
          return [ AnfFun binding ]
      | ValBinding vb -> anf_of_let_binding vb >>| List.rev >>| List.map aval
    in
    let im_res = monadic_map p inner |> run in
    List.concat im_res |> List.partition is_val_binding
    |> fun (main_statements, function_bindings) ->
    function_bindings @ main_statements
end

module AnfOptimizations = struct
  open AnfTypeAst

  let optimize_moves (p : anf_program) = p
end
