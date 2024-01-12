(** Copyright 2023-2024, Danila Pechenev, Ilya Dudnikov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Anf
open Format

let pp_id fmt = function
  | AnfId id -> fprintf fmt "i%d" id
  | GlobalScopeId id -> fprintf fmt "%s" id
;;

let rec pp_immexpr fmt =
  let pp_list pp fmt delimiter =
    pp_print_list
      ~pp_sep:(fun fmt _ -> fprintf fmt delimiter)
      (fun fmt value -> pp fmt value)
      fmt
  in
  function
  | ImmInt num -> fprintf fmt "%d" num
  | ImmString str -> fprintf fmt "\"%s\"" str
  | ImmChar smb -> fprintf fmt "'%c'" smb
  | ImmBool boolean -> fprintf fmt (if boolean then "true" else "false")
  | ImmList imm_values ->
    fprintf fmt "[%a]" (fun fmt -> pp_list pp_immexpr fmt "; ") imm_values
  | ImmTuple imm_values ->
    fprintf fmt "(%a)" (fun fmt -> pp_list pp_immexpr fmt ", ") imm_values
  | ImmId id -> pp_id fmt id
;;

let rec pp_cexpr fmt = function
  | CBinaryOperation (bop, left, right) ->
    fprintf
      fmt
      "%a %a %a"
      pp_immexpr
      left
      Pprintast.pp_binary_operator
      bop
      pp_immexpr
      right
  | CUnaryOperation (unop, imm) ->
    fprintf fmt "%a%a" Pprintast.pp_unary_operator unop pp_immexpr imm
  | CApplication (fun_imm, arg_imm) ->
    fprintf fmt "%a %a" pp_immexpr fun_imm pp_immexpr arg_imm
  | CIf (condition, true_branch, false_branch) ->
    fprintf
      fmt
      "if %a then %a else %a"
      pp_immexpr
      condition
      pp_aexpr
      true_branch
      pp_aexpr
      false_branch
  | CConstructList (head, tail) -> fprintf fmt "%a :: %a" pp_immexpr head pp_immexpr tail
  | CImm immexpr -> pp_immexpr fmt immexpr

and pp_aexpr fmt = function
  | ALet (id, cexpr, aexpr) ->
    fprintf fmt "let %a =\n  %a in\n  %a" pp_id id pp_cexpr cexpr pp_aexpr aexpr
  | ACExpr cexpr -> pp_cexpr fmt cexpr
;;

let pp_global_scope_function fmt =
  let pp_args pp fmt =
    pp_print_list
      ~pp_sep:(fun fmt _ -> fprintf fmt " ")
      (fun fmt value -> pp fmt value)
      fmt
  in
  function
  | name, arg_list, body ->
    (match arg_list with
     | [] -> Format.fprintf fmt "let %s = %a" name pp_aexpr body
     | arg_list ->
       Format.fprintf
         fmt
         "let %s %a = %a"
         name
         (fun fmt -> pp_args pp_id fmt)
         arg_list
         pp_aexpr
         body)
;;

let%expect_test _ =
  printf "%a" pp_immexpr @@ ImmInt 2;
  [%expect {|
  2
  |}]
;;

let%expect_test _ =
  printf "%a" pp_immexpr @@ ImmString "abacaba";
  [%expect {|
  "abacaba"
  |}]
;;

let%expect_test _ =
  printf "%a" pp_immexpr @@ ImmId (AnfId 1);
  [%expect {|
  i1
  |}]
;;

let%expect_test _ =
  printf "%a" pp_cexpr @@ CBinaryOperation (Add, ImmInt 2, ImmInt 3);
  [%expect {|
  2 + 3
  |}]
;;

let%expect_test _ =
  printf "%a" pp_cexpr
  @@ CBinaryOperation (GT, ImmString "greater_than", ImmString "less_than");
  [%expect {|
  "greater_than" > "less_than"
  |}]
;;

let%expect_test _ =
  printf "%a" pp_aexpr
  @@ ALet
       ( AnfId 1
       , CBinaryOperation (Mul, ImmInt 0, ImmInt 100)
       , ACExpr (CImm (ImmId (AnfId 1))) );
  [%expect {|
  let i1 =
    0 * 100 in
    i1
  |}]
;;

let%expect_test _ =
  printf "%a" pp_aexpr
  @@ ALet
       ( AnfId 1
       , CBinaryOperation (Mul, ImmInt 0, ImmInt 100)
       , ALet
           ( AnfId 2
           , CBinaryOperation (Sub, ImmId (AnfId 1), ImmInt 10)
           , ACExpr (CBinaryOperation (Eq, ImmId (AnfId 2), ImmId (AnfId 1))) ) );
  [%expect {|
  let i1 =
    0 * 100 in
    let i2 =
    i1 - 10 in
    i2 = i1
  |}]
;;
