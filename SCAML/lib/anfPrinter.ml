(** Copyright 2023-2024, Grigory Aseev and Matvey Kalashnikov *)

(** SPDX-License-Identifier: LGPL-3.0 *)

open RestrictedAst
open Format
open Ast

let pp_immexpr ppf = function
  | ImmNum n -> fprintf ppf "%n" n
  | ImmId id -> fprintf ppf "%s" id
  | ImmBool b ->
    let bs = if b then "true" else "false" in
    fprintf ppf "%s" bs
  | ImmUnit -> fprintf ppf "()"
;;

let pp_binop ppf = function
  | Add -> fprintf ppf "+"
  | Sub -> fprintf ppf "-"
  | Mul -> fprintf ppf "*"
  | Div -> fprintf ppf "/"
  | Mod -> fprintf ppf "%s" "%"
  | Less -> fprintf ppf "<"
  | Leq -> fprintf ppf "<="
  | Gre -> fprintf ppf ">"
  | Geq -> fprintf ppf ">="
  | Eq -> fprintf ppf "="
  | Neq -> fprintf ppf "<>"
  | And -> fprintf ppf "&&"
  | Or -> fprintf ppf "||"
;;

let pp_pexpr ppf = function
  | PImmExpr i -> fprintf ppf "%a" pp_immexpr i
  | PImmWild -> fprintf ppf "_"
;;

let rec pp_cexpr ppf = function
  | CBinOp (op, l, r) -> fprintf ppf "%a %a %a" pp_immexpr l pp_binop op pp_immexpr r
  | CImmExpr i -> fprintf ppf "%a" pp_immexpr i
  | CApp (l, r) -> fprintf ppf "%a %a" pp_immexpr l pp_immexpr r
  | CIf (cond, t, e) ->
    fprintf ppf "if %a then %a else %a" pp_immexpr cond pp_aexpr t pp_aexpr e

and pp_aexpr ppf = function
  | ALetIn (name, value, ae) ->
    fprintf ppf "let %s = %a in\n %a" name pp_cexpr value pp_aexpr ae
  | ACExpr ce -> fprintf ppf "%a" pp_cexpr ce
;;

let pp_list ppf pp sep =
  pp_print_list ~pp_sep:(fun ppf _ -> fprintf ppf sep) (fun ppf value -> pp ppf value) ppf
;;

let pp_bexpr ppf = function
  | ALet (r, name, args, ae) ->
    let rs = if r then "rec " else "" in
    fprintf
      ppf
      "let %s%s %a = %a"
      rs
      name
      (fun ppf -> pp_list ppf pp_pexpr " ")
      args
      pp_aexpr
      ae
;;

let pp_prexpr ppf binds = fprintf ppf "%a" (fun ppf -> pp_list ppf pp_bexpr ";;\n") binds
