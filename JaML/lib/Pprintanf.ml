(** Copyright 2024, Ilya Pankratov, Maxim Drumov *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open Anf
open Stdlib.Format

let pp_name ppf = fprintf ppf "%s"
let space ppf depth = fprintf ppf "\n%*s" (4 * depth) ""

let pp_tuple ppf pp_el =
  fprintf ppf "(%a)" (pp_print_list ~pp_sep:(fun ppf _ -> fprintf ppf ", ") pp_el)
;;

let pp_args pp_el ppf =
  fprintf ppf "%a" (pp_print_list ~pp_sep:(fun ppf _ -> fprintf ppf " ") pp_el)
;;

let pp_immexpr ppf = function
  | ImmNum i -> fprintf ppf "%d" i
  | ImmBool b -> fprintf ppf "%B" b
  | ImmId s -> fprintf ppf "%s" s
;;

let rec pp_aexpr tabs ppf =
  let helper = pp_aexpr 1 in
  function
  | ALet (name, cexpr, aexpr) ->
    fprintf ppf "%alet %a = %a in %a" space tabs pp_name name pp_cexpr cexpr helper aexpr
  | ACEexpr cexpr -> fprintf ppf "%a" pp_cexpr cexpr

and pp_cexpr ppf =
  let pp_aexpr = pp_aexpr 1 in
  let pp_args = pp_args pp_immexpr in
  function
  | CBinOp (op, l, r) ->
    fprintf ppf "(%a %a %a)" pp_immexpr l Ast.pp_bin_op op pp_immexpr r
  | CApp (func, args) ->
    if Base.List.is_empty args
    then fprintf ppf "empty_app(%a)" pp_immexpr func
    else fprintf ppf "%a %a" pp_immexpr func pp_args args
  | CImmExpr imm -> fprintf ppf "%a" pp_immexpr imm
  | CTake (imm, n) -> fprintf ppf "take(%a, %i)" pp_immexpr imm n
  | CMakeClosure (imm, _, _, args) ->
    if Base.List.is_empty args
    then fprintf ppf "make_empty_closure(%a)" pp_immexpr imm
    else fprintf ppf "make_closure(%a %a)" pp_immexpr imm pp_args args
  | CAddArgsToClosure (imm, _, args) ->
    fprintf ppf "add_args_to_closure(%a %a)" pp_immexpr imm pp_args args
  | CTuple elems -> pp_tuple ppf pp_immexpr elems
  | CIfThenElse (if_aexpr, then_aexpr, else_aexpr) ->
    fprintf
      ppf
      "(if %a then %a else %a)"
      pp_immexpr
      if_aexpr
      pp_aexpr
      then_aexpr
      pp_aexpr
      else_aexpr
;;

let pp_anfexpr ppf (AnfLetFun (name, args, aexpr)) =
  let pp_aexpr = pp_aexpr 1 in
  let pp_args =
    pp_args (fun ppf ->
         function
         | Anf.Used s -> fprintf ppf "%s" s
         | _ -> fprintf ppf "_")
  in
  if Base.List.is_empty args
  then fprintf ppf "let %a = %a" pp_name name pp_aexpr aexpr
  else fprintf ppf "let %a %a = %a" pp_name name pp_args args pp_aexpr aexpr
;;

let pp_anfstatements =
  pp_print_list
    ~pp_sep:(fun ppf _ -> fprintf ppf ";\n")
    (fun ppf binding -> pp_anfexpr ppf binding)
;;
