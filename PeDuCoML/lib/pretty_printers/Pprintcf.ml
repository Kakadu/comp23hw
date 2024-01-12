(** Copyright 2023-2024, Danila Pechenev, Ilya Dudnikov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Pprintast
open Lambda_lift
open Format
open Base

let rec pp_cfexpr fmt =
  let get_format = function
    | CFLiteral _ | CFIdentifier _ -> Stdlib.format_of_string "%a"
    | _ -> Stdlib.format_of_string "(%a)"
  in
  let ( ^^ ) = Stdlib.( ^^ ) in
  let pp_list fmt delimiter =
    pp_print_list
      ~pp_sep:(fun fmt _ -> fprintf fmt delimiter)
      (fun fmt value -> pp_cfexpr fmt value)
      fmt
  in
  function
  | CFLiteral literal -> pp_literal fmt literal
  | CFIdentifier id -> fprintf fmt "%s" id
  | CFBinaryOperation (binary_operator, left_operand, right_operand) ->
    let format =
      Stdlib.format_of_string
        (match left_operand, right_operand with
         | CFLiteral _, CFLiteral _
         | CFLiteral _, CFIdentifier _
         | CFIdentifier _, CFLiteral _
         | CFIdentifier _, CFIdentifier _ -> "%a %a %a"
         | (CFLiteral _ | CFIdentifier _), _ -> "%a %a (%a)"
         | _, (CFLiteral _ | CFIdentifier _) -> "(%a) %a %a"
         | _ -> "(%a) %a (%a)")
    in
    fprintf
      fmt
      format
      pp_cfexpr
      left_operand
      pp_binary_operator
      binary_operator
      pp_cfexpr
      right_operand
  | CFUnaryOperation (unary_operator, operand) ->
    fprintf
      fmt
      ("%a" ^^ get_format operand)
      pp_unary_operator
      unary_operator
      pp_cfexpr
      operand
  | CFApplication (left_operand, right_operand) ->
    fprintf
      fmt
      (get_format left_operand ^^ " " ^^ get_format right_operand)
      pp_cfexpr
      left_operand
      pp_cfexpr
      right_operand
  | CFList list -> fprintf fmt "[%a]" (fun fmt -> pp_list fmt "; ") list
  | CFConstructList (operand, list) ->
    fprintf
      fmt
      (get_format operand ^^ " :: " ^^ get_format list)
      pp_cfexpr
      operand
      pp_cfexpr
      list
  | CFTuple (first_elem, second_elem, other_elems) ->
    pp_list fmt ", " (first_elem :: second_elem :: other_elems)
  | CFIf (predicate, true_branch, false_branch) ->
    fprintf
      fmt
      "if %a then %a else %a"
      pp_cfexpr
      predicate
      pp_cfexpr
      true_branch
      pp_cfexpr
      false_branch
  | CFLetIn (first_declaration, other_declarations, body) ->
    let declaration_list = first_declaration :: other_declarations in
    (match declaration_list with
     | [] -> fprintf fmt "Parsing error."
     | head :: tail ->
       fprintf fmt "%a" pp_cflet head;
       List.iter tail ~f:(fun declaration -> pp_cflet fmt declaration);
       fprintf fmt " in %a" pp_cfexpr body)
  | CFMatchWith (matched_expression, first_case, other_cases) ->
    fprintf fmt "match %a with" pp_cfexpr matched_expression;
    List.iter (first_case :: other_cases) ~f:(fun (case, action) ->
      fprintf fmt " | %a -> %a" pp_pattern case pp_cfexpr action)

and pp_cflet fmt (id, body) = fprintf fmt "let %s = %a" id pp_cfexpr body

let pp_declaration fmt =
  let pp_args fmt =
    pp_print_list
      ~pp_sep:(fun fmt _ -> fprintf fmt " ")
      (fun fmt arg -> fprintf fmt "%s" arg)
      fmt
  in
  function
  | CFDeclaration (name, arg_list, expression) ->
    fprintf fmt "let %s %a = %a" name pp_args arg_list pp_cfexpr expression
  | CFRecursiveDeclaration (name, arg_list, expression) ->
    fprintf fmt "let rec %s %a = %a" name pp_args arg_list pp_cfexpr expression
;;
