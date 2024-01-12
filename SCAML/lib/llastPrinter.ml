(** Copyright 2023-2024, Grigory Aseev and Matvey Kalashnikov *)

(** SPDX-License-Identifier: LGPL-3.0 *)

open AstPrinter
open Llast
open Format

let rec pp_llexpr fmt = function
  | LLConst c -> pp_const fmt c
  | LLVar v -> fprintf fmt "%s" v
  | LLBinOp (op, l1, l2) ->
    fprintf fmt "(%a %a %a)" pp_llexpr l1 pp_bin_op op pp_llexpr l2
  | LLIf (l1, l2, l3) ->
    fprintf fmt "(if %a then %a else %a)" pp_llexpr l1 pp_llexpr l2 pp_llexpr l3
  | LLApp (l1, l2) -> fprintf fmt "(%a %a)" pp_llexpr l1 pp_llexpr l2
  | LLLetIn (id, l1, l2) -> fprintf fmt "let %s = %a in %a" id pp_llexpr l1 pp_llexpr l2
;;

let pp_llbinding fmt = function
  | LLLet (rec_flag, id, patterns, l) ->
    fprintf
      fmt
      "let %a %s %a = %a"
      pp_rec_flag
      rec_flag
      id
      pp_patterns
      patterns
      pp_llexpr
      l
;;
