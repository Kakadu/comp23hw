(** Copyright 2023-2024, Anton Kraev and Polina Badreeva *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Anf
open Ast

let string_of_unary_op = function
  | Neg -> "neg"
  | Not -> "not"
;;

let string_of_binary_op = function
  | Add -> "+"
  | Sub -> "-"
  | Mult -> "*"
  | Div -> "/"
  | Eq -> "="
  | Lt -> "<"
  | LtOrEq -> "<="
  | Gt -> ">"
  | GtOrEq -> ">="
  | And -> "and"
  | Or -> "or"
;;

let string_of_indents n = String.make (n * 2) ' '

let string_of_immexpr = function
  | ImmInt n -> string_of_int n
  | ImmBool b -> string_of_bool b
  | ImmIdentifier s -> s
;;

let rec string_of_cexpr depth = function
  | CImmExpr ie -> string_of_immexpr ie
  | CUnaryOp (op, ie) ->
    Printf.sprintf "%s %s" (string_of_unary_op op) (string_of_immexpr ie)
  | CBinaryOp (op, ie1, ie2) ->
    Printf.sprintf
      "(%s %s %s)"
      (string_of_immexpr ie1)
      (string_of_binary_op op)
      (string_of_immexpr ie2)
  | CApp (ie1, ie2) ->
    Printf.sprintf "%s %s" (string_of_immexpr ie1) (string_of_immexpr ie2)
  | CIfThenElse (ie1, ie2, ie3) ->
    Printf.sprintf
      "if %s then \n%s else \n%s"
      (string_of_immexpr ie1)
      (string_of_aexpr depth ie2)
      (string_of_aexpr depth ie3)

and string_of_aexpr depth = function
  | ALet (id, ce, ae) ->
    Printf.sprintf
      "%slet %s = %s in\n%s"
      (string_of_indents depth)
      id
      (string_of_cexpr (depth + 1) ce)
      (string_of_aexpr depth ae)
  | ACExpr ce -> string_of_cexpr (depth + 1) ce
;;

let string_of_abinding' depth = function
  | AFun (id, id_list, ae) ->
    Printf.sprintf
      "%sfun %s %s = %s%s\n"
      (string_of_indents depth)
      id
      (String.concat " " id_list)
      (match ae with
       | ALet _ -> "\n"
       | _ -> "")
      (string_of_aexpr (depth + 1) ae)
;;

let string_of_abinding ab = string_of_abinding' 0 ab
