(** Copyright 2023-2024, Arthur Alekseev and Starcev Matvey *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

module PrettyPrinter = struct
  open Closure.ClosureAst
  open Anf.AnfTypeAst
  open Type_ast.TypeAst
  open Parser_ast.ParserAst
  open Indexed_ast.IndexedTypeAst

  (********************************************************
                         Pretty print CF AST
    ********************************************************)
  let show_literal = function
    | LInt i -> Format.sprintf "%d" i
    | LFloat f -> Format.sprintf "%f" f
    | LString s -> Format.sprintf "%S" s
    | LBool b -> if b then "true" else "false"
    | LUnit -> "()"

  let rec print_cf_expr st (e : cf_typ_expr) =
    match e.value with
    | CFApply (x, args) ->
        let args = List.map (print_cf_expr st) args |> String.concat " " in
        Format.sprintf "(%s %s)" (print_cf_expr st x) args
    | CFTuple xs ->
        List.map (print_cf_expr st) xs
        |> String.concat ", " |> Format.sprintf "(%s)"
    | CFIfElse ite ->
        let i = ite.cond in
        let t = ite.t_body in
        let e = ite.f_body in
        Format.sprintf "if %s then %s else %s" (print_cf_expr st i)
          (print_cf_expr st t) (print_cf_expr st e)
    | CFValue v -> Format.sprintf "%s" v
    | CFLiteral l -> show_literal l
    | CFClosure (i, env) ->
        let env =
          String.concat ", "
          @@ List.map (fun x -> Format.sprintf "%s" x.value) env
        in
        Format.sprintf "clsr[%s][%s]" i env

  let rec print_lval = function
    | DLvValue v -> Format.sprintf "%s" v
    | DLvAny -> "any"
    | DLvUnit -> "()"
    | DLvTuple xs ->
        List.map print_lval xs |> String.concat ", " |> Format.sprintf "(%s)"

  let rec print_cf_dec st intd = function
    | ValBinding x ->
        let lval = print_lval x.l_v.value in
        let lets =
          List.fold_left
            (fun xs x -> xs ^ "\n" ^ print_cf_dec st (intd ^ "  ") x)
            intd
            (List.map (fun x -> ValBinding x) x.cf_body.cf_lets)
        in
        Format.sprintf "%slet %s%s =%s\n%s  %s" intd lval (st x.l_v.typ) lets
          intd
          (print_cf_expr st x.cf_body.cf_expr)
    | FunBinding x ->
        let lval = x.name.value in
        let lets =
          List.fold_left
            (fun xs x -> xs ^ "\n" ^ print_cf_dec st (intd ^ "  ") x)
            intd
            (List.map (fun x -> ValBinding x) x.b.cf_lets)
        in
        let args =
          List.fold_left
            (fun xs (x : index_lvalue) ->
              Format.sprintf "%s (%s%s)" xs (print_lval x.value) (st x.typ))
            "" x.args
        in
        Format.sprintf "%sletc %s%s%s =%s\n%s  %s" intd lval args
          (st x.name.typ) lets intd
          (print_cf_expr st x.b.cf_expr)

  let print_cf_program print_type program =
    let do_show_type t = ": " ^ show_ty t in
    let dont_show_type _ = "" in
    let st = if print_type then do_show_type else dont_show_type in
    List.map (print_cf_dec st "") program |> String.concat "\n\n"

  (********************************************************
                       Pretty print ANF AST
    ********************************************************)
  let print_imm st = function
    | ImmVal x -> Format.sprintf "%s%s" x.value (st x.typ)
    | ImmLit x -> Format.sprintf "%s%s" (show_literal x.value) (st x.typ)

  let rec print_body st intd (b : anf_body) =
    let inner xs x = xs ^ "\n" ^ print_anf_dec st (intd ^ "  ") (AnfVal x) in
    let lets = List.fold_left inner "" b.lets in
    let expr = Format.sprintf "  %s%s" intd (print_imm st b.res) in
    Format.sprintf "%s\n%s" lets expr

  and print_anf_expr st intd = function
    | AApply (x, y) ->
        let args = List.map (print_imm st) y |> String.concat " " in
        Format.sprintf "%s %s" (print_imm st x) args
    | ATuple xs ->
        List.map (print_imm st) xs
        |> String.concat ", " |> Format.sprintf "(%s)"
    | Aite (i, t, e) ->
        let i_b = print_imm st i in
        let t_b = print_body st intd t in
        let f_b = print_body st intd e in
        Format.sprintf "if %s then%s\n%selse%s" i_b t_b intd f_b
    | ATupleAccess (t, e) -> Format.sprintf "%s[%d]" (print_imm st t) e
    | AImm i -> print_imm st i
    | AClosure (i, env) ->
        let env = String.concat ", " @@ List.map (print_imm st) env in
        Format.sprintf "clsr[%s%s][%s]" i.value (st i.typ) env

  and print_anf_dec st intd = function
    | AnfVal x ->
        Format.sprintf "%slet (%s%s) = %s" intd x.name.value (st x.name.typ)
          (print_anf_expr st intd x.e)
    | AnfFun x ->
        let intd = intd ^ "  " in
        let inner xs x = xs ^ "\n" ^ print_anf_dec st intd (AnfVal x) in
        let name, name_type = (x.name.value, st x.name.typ) in
        let args =
          List.map (fun x -> Format.sprintf "(%s%s)" x.value (st x.typ)) x.args
          |> String.concat " "
        in
        let env =
          List.map (fun x -> Format.sprintf "[%s%s]" x.value (st x.typ)) x.env
          |> String.concat " "
        in
        let lets = List.fold_left inner "" x.body.lets in
        Format.sprintf "let (%s%s) %s %s =%s\n%s%s" name name_type env args lets
          intd (print_imm st x.body.res)

  let print_anf_program print_type program =
    let do_show_type t = ": " ^ show_ty t in
    let dont_show_type _ = "" in
    let st = if print_type then do_show_type else dont_show_type in
    List.map (print_anf_dec st "") program |> String.concat "\n\n"

  (********************************************************
                        Pretty print DB AST
    ********************************************************)
  let rec print_index_expr st intd e =
    match e.value with
    | DApply (l, r) ->
        Format.sprintf "(%s %s)"
          (print_index_expr st intd l)
          (print_index_expr st intd r)
    | DLiteral l -> show_literal l
    | DValue v -> Format.sprintf "%s" v
    | DFun fn ->
        let lval = print_lval fn.lvalue.value in
        let lets =
          List.fold_left
            (fun xs x -> xs ^ "\n" ^ print_index_let_binding st (intd ^ "  ") x)
            intd fn.b.lets
        in
        let expr = print_index_expr st intd fn.b.expr in
        Format.sprintf "fun (%s%s) -> %s\n%s  %s" lval (st fn.lvalue.typ) lets
          intd expr
    | DTuple vs ->
        List.map (print_index_expr st intd) vs
        |> String.concat ", " |> Format.sprintf "(%s)"
    | DIfElse ite ->
        let i_b = print_index_expr st intd ite.cond in
        let t_b = print_index_expr st intd ite.t_body in
        let f_b = print_index_expr st intd ite.f_body in
        Format.sprintf "if %s then %s else %s" i_b t_b f_b

  and print_index_let_binding st intd (x : index_let_binding) =
    let lval = print_lval x.l_v.value in
    let lets =
      List.fold_left
        (fun xs x -> xs ^ "\n" ^ print_index_let_binding st (intd ^ "  ") x)
        intd x.body.lets
    in
    let expr = print_index_expr st intd x.body.expr in
    let t = st x.l_v.typ in
    Format.sprintf "%slet %s%s = %s\n%s  %s" intd lval t lets intd expr

  let print_index_program print_type program =
    let do_show_type t = ": " ^ show_ty t in
    let dont_show_type _ = "" in
    let st = if print_type then do_show_type else dont_show_type in
    List.map (print_index_let_binding st "") program |> String.concat "\n\n"

  (********************************************************
                        Pretty print Typed AST
    ********************************************************)
  let rec print_llval = function
    | LvAny -> "_"
    | LvUnit -> "()"
    | LvValue v -> v
    | LvTuple vs ->
        List.map (fun x -> Position.Position.value x |> print_llval) vs
        |> String.concat ", " |> Format.sprintf "(%s)"

  let rec print_typ_expr st intd e =
    match e.value with
    | TApply (l, r) ->
        Format.sprintf "(%s %s)" (print_typ_expr st intd l)
          (print_typ_expr st intd r)
    | TLiteral l -> show_literal l
    | TValue v -> Format.sprintf "%s" v
    | TFun fn ->
        let lval = print_llval fn.lvalue.value in
        let lets =
          List.fold_left
            (fun xs x -> xs ^ "\n" ^ print_typ_let_binding st (intd ^ "  ") x)
            intd fn.b.lets
        in
        let expr = print_typ_expr st intd fn.b.expr in
        Format.sprintf "fun %s%s -> %s\n%s  %s" lval (st fn.lvalue.typ) lets
          intd expr
    | TTuple vs ->
        List.map (print_typ_expr st intd) vs
        |> String.concat ", " |> Format.sprintf "(%s)"
    | TIfElse ite ->
        let i_b = print_typ_expr st intd ite.cond in
        let t_b = print_typ_expr st intd ite.t_body in
        let f_b = print_typ_expr st intd ite.f_body in
        Format.sprintf "if %s then %s else %s" i_b t_b f_b

  and print_typ_let_binding st intd (x : ty typ_let_binding) =
    let lval = print_llval x.l_v.value in
    let lets =
      List.fold_left
        (fun xs x -> xs ^ "\n" ^ print_typ_let_binding st (intd ^ "  ") x)
        intd x.body.lets
    in
    let expr = print_typ_expr st intd x.body.expr in
    let t = st x.l_v.typ in
    Format.sprintf "%slet %s%s = %s\n%s  %s" intd lval t lets intd expr

  let print_typ_program print_type program =
    let do_show_type t = ": " ^ show_ty t in
    let dont_show_type _ = "" in
    let st = if print_type then do_show_type else dont_show_type in
    List.map (print_typ_let_binding st "") program |> String.concat "\n\n"
end
