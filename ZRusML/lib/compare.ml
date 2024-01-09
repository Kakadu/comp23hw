(** Copyright 2023-2024, Rustam Shangareev and Danil Yevdokimov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

open Ast

let print_bin_op = function
  | And -> "And"
  | Or -> "Or"
  | Less -> "Less"
  | Leq -> "Leq"
  | Gre -> "Gre"
  | Geq -> "Geq"
  | Eq -> "Eq"
  | Neq -> "Neq"
  | Add -> "Add"
  | Sub -> "Sub"
  | Mul -> "Mul"
  | Div -> "Div"

and print_un_op = function
  | Not -> "Not"
  | Minus -> "Minus"
;;

let rec print_pt = function
  | PtWild -> "_"
  | PtVar v -> Printf.sprintf "PtVar(%s)" v
  | PtConst c -> print_const c

and print_const = function
  | CInt i -> Printf.sprintf "CInt(%d)" i
  | CBool b -> Printf.sprintf "CBool(%b)" b
;;

let rec print_exp = function
  | EConst c -> print_const c
  | EVar v -> Printf.sprintf "EVar(%s)" v
  | EUnOp (op, e) -> Printf.sprintf "EUnOp(%s, %s)" (print_un_op op) (print_exp e)
  | EBinOp (op, e1, e2) ->
    Printf.sprintf "EBinOp(%s, %s, %s)" (print_bin_op op) (print_exp e1) (print_exp e2)
  | EIf (e1, e2, e3) ->
    Printf.sprintf "EIf(%s, %s, %s)" (print_exp e1) (print_exp e2) (print_exp e3)
  | ELet (bindings, e) ->
    Printf.sprintf
      "ELet(%s, %s)"
      (String.concat ", " (List.map print_let bindings))
      (print_exp e)
  | EFun (pt, e) -> Printf.sprintf "EFun(%s, %s)" (print_pt pt) (print_exp e)
  | EApp (e1, e2) -> Printf.sprintf "EApp(%s, %s)" (print_exp e1) (print_exp e2)

and print_let (is_rec, pt, exp) =
  Printf.sprintf "(%b, %s, %s)" is_rec (print_pt pt) (print_exp exp)

and print_bindings bindings =
  let print_binding (b, pt, e) =
    Printf.sprintf "(%b, %s, %s)" b (print_pt pt) (print_exp e)
  in
  "[" ^ String.concat "; " (List.map print_binding bindings) ^ "]"
;;

let rec print_prog prog =
  String.concat
    "\n"
    (List.map
       (function
         | DLet (is_rec, pt, exp) ->
           Printf.sprintf "DLet(%b, %s, %s)" is_rec (print_pt pt) (print_exp exp))
       prog)
;;
