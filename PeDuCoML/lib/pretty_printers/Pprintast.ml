(** Copyright 2023-2024, Danila Pechenev, Ilya Dudnikov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Format
open Base

let pp_literal fmt = function
  | LInt x -> fprintf fmt "%d" x
  | LString x -> fprintf fmt "%s" x
  | LChar x -> fprintf fmt "%c" x
  | LBool x -> fprintf fmt "%b" x
;;

let pp_binary_operator fmt = function
  | Add -> fprintf fmt "+"
  | Sub -> fprintf fmt "-"
  | Mul -> fprintf fmt "*"
  | Div -> fprintf fmt "/"
  | Eq -> fprintf fmt "="
  | NEq -> fprintf fmt "<>"
  | GT -> fprintf fmt ">"
  | GTE -> fprintf fmt "≥"
  | LT -> fprintf fmt "<"
  | LTE -> fprintf fmt "≤"
  | AND -> fprintf fmt "&&"
  | OR -> fprintf fmt "||"
;;

let pp_unary_operator fmt = function
  | Minus -> fprintf fmt "-"
  | Not -> fprintf fmt "not "
;;

let rec pp_pattern fmt =
  let open Stdlib in
  let get_format = function
    | PLiteral _ | PIdentifier _ -> format_of_string "%a"
    | _ -> format_of_string "(%a)"
  in
  let pp_list fmt delimiter =
    pp_print_list
      ~pp_sep:(fun fmt _ -> fprintf fmt delimiter)
      (fun fmt value -> pp_pattern fmt value)
      fmt
  in
  function
  | PLiteral literal -> pp_literal fmt literal
  | PWildcard -> fprintf fmt "_"
  | PTuple (first_pattern, second_pattern, other_patterns) ->
    pp_list fmt ", " (first_pattern :: second_pattern :: other_patterns)
  | PList list -> fprintf fmt "[%a]" (fun fmt -> pp_list fmt "; ") list
  | PConstructList (operand, list) ->
    fprintf
      fmt
      (get_format operand ^^ " :: " ^^ get_format list)
      pp_pattern
      operand
      pp_pattern
      list
  | PIdentifier id -> fprintf fmt "%s" id
;;

let rec pp_expression fmt =
  let get_format = function
    | ELiteral _ | EIdentifier _ -> Stdlib.format_of_string "%a"
    | _ -> Stdlib.format_of_string "(%a)"
  in
  let ( ^^ ) = Stdlib.( ^^ ) in
  let pp_list fmt delimiter =
    pp_print_list
      ~pp_sep:(fun fmt _ -> fprintf fmt delimiter)
      (fun fmt value -> pp_expression fmt value)
      fmt
  in
  let pp_pattern_list fmt =
    pp_print_list
      ~pp_sep:(fun fmt _ -> fprintf fmt " ")
      (fun fmt pattern -> fprintf fmt "%a" pp_pattern pattern)
      fmt
  in
  function
  | ELiteral literal -> pp_literal fmt literal
  | EIdentifier id -> fprintf fmt "%s" id
  | EBinaryOperation (binary_operator, left_operand, right_operand) ->
    let format =
      Stdlib.format_of_string
        (match left_operand, right_operand with
         | ELiteral _, ELiteral _
         | ELiteral _, EIdentifier _
         | EIdentifier _, ELiteral _
         | EIdentifier _, EIdentifier _ -> "%a %a %a"
         | (ELiteral _ | EIdentifier _), _ -> "%a %a (%a)"
         | _, (ELiteral _ | EIdentifier _) -> "(%a) %a %a"
         | _ -> "(%a) %a (%a)")
    in
    fprintf
      fmt
      format
      pp_expression
      left_operand
      pp_binary_operator
      binary_operator
      pp_expression
      right_operand
  | EUnaryOperation (unary_operator, operand) ->
    fprintf
      fmt
      ("%a" ^^ get_format operand)
      pp_unary_operator
      unary_operator
      pp_expression
      operand
  | EApplication (left_operand, right_operand) ->
    fprintf
      fmt
      (get_format left_operand ^^ " " ^^ get_format right_operand)
      pp_expression
      left_operand
      pp_expression
      right_operand
  | EFun (first_arg, other_args, fun_body) ->
    fprintf
      fmt
      "fun %a -> %a"
      pp_pattern_list
      (first_arg :: other_args)
      pp_expression
      fun_body
  | EList list -> fprintf fmt "[%a]" (fun fmt -> pp_list fmt "; ") list
  | EConstructList (operand, list) ->
    fprintf
      fmt
      (get_format operand ^^ " :: " ^^ get_format list)
      pp_expression
      operand
      pp_expression
      list
  | ETuple (first_elem, second_elem, other_elems) ->
    pp_list fmt ", " (first_elem :: second_elem :: other_elems)
  | EIf (predicate, true_branch, false_branch) ->
    fprintf
      fmt
      "if %a then %a else %a"
      pp_expression
      predicate
      pp_expression
      true_branch
      pp_expression
      false_branch
  | ELetIn (first_declaration, other_declarations, body) ->
    let declaration_list = first_declaration :: other_declarations in
    (match declaration_list with
     | [] -> fprintf fmt "Parsing error."
     | head :: tail ->
       fprintf fmt "%a" pp_declaration head;
       List.iter tail ~f:(fun declaration -> pp_declaration fmt declaration);
       fprintf fmt " in %a" pp_expression body)
  | EMatchWith (matched_expression, first_case, other_cases) ->
    fprintf fmt "match %a with" pp_expression matched_expression;
    List.iter (first_case :: other_cases) ~f:(fun (case, action) ->
      fprintf fmt " | %a -> %a" pp_pattern case pp_expression action)

and pp_declaration fmt =
  let pp_pattern_list fmt =
    pp_print_list
      ~pp_sep:(fun fmt _ -> fprintf fmt " ")
      (fun fmt pattern -> fprintf fmt "%a" pp_pattern pattern)
      fmt
  in
  function
  | DDeclaration (name, pattern_list, expression) ->
    fprintf
      fmt
      "let %s %a = %a"
      name
      pp_pattern_list
      pattern_list
      pp_expression
      expression
  | DRecursiveDeclaration (name, pattern_list, expression) ->
    fprintf
      fmt
      "let rec %s %a = %a"
      name
      pp_pattern_list
      pattern_list
      pp_expression
      expression
;;

let%expect_test _ =
  printf "%a" pp_expression
  @@ EFun
       ( PIdentifier "x"
       , [ PIdentifier "y"; PIdentifier "z" ]
       , EMatchWith
           ( ETuple (EIdentifier "x", EIdentifier "y", [ EIdentifier "z" ])
           , ( PTuple
                 (PLiteral (LBool true), PLiteral (LBool true), [ PLiteral (LBool false) ])
             , ELiteral (LBool true) )
           , [ ( PTuple
                   ( PLiteral (LBool true)
                   , PLiteral (LBool false)
                   , [ PLiteral (LBool true) ] )
               , ELiteral (LBool true) )
             ; ( PTuple
                   ( PLiteral (LBool false)
                   , PLiteral (LBool true)
                   , [ PLiteral (LBool true) ] )
               , ELiteral (LBool true) )
             ; PWildcard, ELiteral (LBool false)
             ] ) );
  [%expect
    {|
  fun x y z -> match x, y, z with | true, true, false -> true | true, false, true -> true | false, true, true -> true | _ -> false
  |}]
;;

let%expect_test _ =
  printf "%a" pp_expression
  @@ EFun
       ( PIdentifier "line"
       , [ PIdentifier "number"; PIdentifier "line_mult_number" ]
       , EMatchWith
           ( EIdentifier "line"
           , ( PConstructList (PIdentifier "head", PIdentifier "tail")
             , EConstructList
                 ( EBinaryOperation (Mul, EIdentifier "head", EIdentifier "number")
                 , EApplication (EIdentifier "line_mult_number", EIdentifier "tail") ) )
           , [ PWildcard, EList [] ] ) );
  [%expect
    {|
  fun line number line_mult_number -> match line with | head :: tail -> (head * number) :: (line_mult_number tail) | _ -> []
  |}]
;;
