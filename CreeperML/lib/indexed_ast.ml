(** Copyright 2023-2024, Arthur Alekseev and Starcev Matvey *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

module IndexedTypeAst = struct
  open Type_ast.TypeAst
  open Parser_ast.ParserAst
  open Position.Position
  open Counter

  type ilvalue = DLvAny | DLvUnit | DLvValue of int | DLvTuple of ilvalue list
  type index_lvalue = (ilvalue, ty) typed

  type index_let_binding = {
    rec_f : rec_flag;
    l_v : index_lvalue;
    body : index_let_body;
  }

  and index_let_body = { lets : index_let_binding list; expr : index_expr }

  and d_expr =
    | DApply of index_expr * index_expr
    | DLiteral of literal
    | DValue of int
    | DFun of index_fun_body
    | DTuple of index_expr list
    | DIfElse of tif_else

  and index_expr = (d_expr, ty) typed
  and index_fun_body = { lvalue : index_lvalue; b : index_let_body }
  and tif_else = { cond : index_expr; t_body : index_expr; f_body : index_expr }

  type index_program = index_let_binding list

  (* END AST, BEGIN TRANSLATE*)

  module NameMap = Map.Make (String)

  type nm = int NameMap.t
  type 'a res = 'a * nm

  (* cons *)
  let typed typ value : ('a, 'b) typed = { typ; value }

  let rec names_of_lvalue (l : lvalue) =
    match l with
    | LvAny | LvUnit -> [ "any" ]
    | LvValue v -> [ v ]
    | LvTuple vs ->
        List.map (fun x -> value x |> names_of_lvalue) vs |> List.concat

  let rec index_lv (nm : nm) (l : lvalue) =
    match l with
    | LvAny -> DLvAny
    | LvUnit -> DLvUnit
    | LvValue v -> DLvValue (NameMap.find v nm)
    | LvTuple vs ->
        List.map (fun x -> value x |> index_lv nm) vs |> fun x -> DLvTuple x

  let rec index_expr (nm : nm) (e : ty typ_expr) : index_expr =
    match e.value with
    | TApply (l, r) ->
        let lr = index_expr nm l in
        let rr = index_expr nm r in
        DApply (lr, rr) |> typed e.typ
    | TIfElse ite ->
        let ir = index_expr nm ite.cond in
        let tr = index_expr nm ite.t_body in
        let er = index_expr nm ite.f_body in
        let cond = ir in
        let t_body = tr in
        let f_body = er in
        DIfElse { cond; t_body; f_body } |> typed e.typ
    | TLiteral l -> DLiteral l |> typed e.typ
    | TValue v -> NameMap.find v nm |> fun x -> DValue x |> typed e.typ
    | TTuple vs ->
        List.map (index_expr nm) vs |> fun x -> DTuple x |> typed e.typ
    | TFun f ->
        let all_names = names_of_lvalue f.lvalue.value in
        let nm =
          List.fold_left
            (fun nm n -> NameMap.add n (cnt_next ()) nm)
            nm all_names
        in
        let lets, nm_inners =
          List.fold_left
            (fun (xs, nm) x ->
              let inner_r, nm = index_let x nm in
              (inner_r :: xs, nm))
            ([], nm) f.b.lets
          |> fun (xs, nm) -> (List.rev xs, nm)
        in
        let expr = index_expr nm_inners f.b.expr in
        let b = { lets; expr } in
        let lvalue = index_lv nm f.lvalue.value |> typed f.lvalue.typ in
        let f = { lvalue; b } in
        DFun f |> typed e.typ

  and index_let (l : ty typ_let_binding) nm : index_let_binding res =
    let all_names = names_of_lvalue l.l_v.value in
    let nm =
      match l.rec_f with
      | Rec ->
          List.fold_left
            (fun nm n -> NameMap.add n (cnt_next ()) nm)
            nm all_names
      | NoRec -> nm
    in
    let lets, nm_inners =
      List.fold_left
        (fun (xs, nm) x ->
          let inner_r, nm = index_let x nm in
          (inner_r :: xs, nm))
        ([], nm) l.body.lets
      |> fun (xs, nm) -> (List.rev xs, nm)
    in
    let expr = l.body.expr |> index_expr nm_inners in
    let body = { lets; expr } in
    let nm =
      match l.rec_f with
      | Rec -> nm
      | NoRec ->
          List.fold_left
            (fun nm n -> NameMap.add n (cnt_next ()) nm)
            nm all_names
    in
    let l_v = index_lv nm l.l_v.value |> typed l.l_v.typ in
    let rec_f = l.rec_f in
    ({ rec_f; body; l_v }, nm)

  (* Move declarations of let inside of fun *)
  let rec move_lets (l : ty typ_let_binding) : ty typ_let_binding =
    match l.body.expr.value with
    | TFun f ->
        let inners = l.body.lets @ f.b.lets in
        let inners = List.map move_lets inners in
        let new_fun = { f with b = { f.b with lets = inners } } in
        let expr = TFun new_fun |> typed l.body.expr.typ in
        { l with body = { lets = []; expr } }
    | _ -> l

  let index_of_typed (nm : nm) (p : ty typ_program) : index_program =
    let p = List.map move_lets p in
    List.fold_left
      (fun (xs, nm) x ->
        let res, nm = index_let x nm in
        (res :: xs, nm))
      ([], nm) p
    |> fst |> List.rev
end
