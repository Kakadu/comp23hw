(** Copyright 2023-2024, Danila Pechenev, Ilya Dudnikov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type id = string [@@deriving eq, show { with_path = false }]

type literal =
  | LInt of int (** 42 *)
  | LString of string (** "42" *)
  | LChar of char (** '\n' *)
  | LBool of bool (** true *)
[@@deriving eq, show { with_path = false }]

type binary_operator =
  | Add (** + *)
  | Sub (** - *)
  | Mul (** * *)
  | Div (** / *)
  | Eq (** = *)
  | NEq (** != *)
  | GT (** > *)
  | GTE (** >= *)
  | LT (** < *)
  | LTE (** <= *)
  | AND (** && *)
  | OR (** || *)
[@@deriving show { with_path = false }]

type unary_operator =
  | Minus (** -1 *)
  | Not (** not true *)
[@@deriving show { with_path = false }]

type pattern =
  | PLiteral of literal (** true *)
  | PWildcard (** _ *)
  | PTuple of pattern * pattern * pattern list (** (1, 2) *)
  | PList of pattern list (** [a; b; c] *)
  | PConstructList of pattern * pattern (** a :: [b; c] *)
  | PIdentifier of id (** cool_variable *)

type expression =
  | ELiteral of literal (** 123 *)
  | EBinaryOperation of binary_operator * expression * expression (** 1 + 3 *)
  | EUnaryOperation of unary_operator * expression (** -(1 + 3) *)
  | EApplication of expression * expression (** f x *)
  | EIdentifier of id (** x *)
  | EFun of pattern * pattern list * expression (** fun x y -> x + y *)
  | EList of expression list (** [ 1; 2; 3 ] *)
  | EConstructList of expression * expression (** 1 :: [2; 3] *)
  | ETuple of expression * expression * expression list (** (1, "Vasya Pupkin", '\n') *)
  | ELetIn of declaration * declaration list * expression
  (** let x = 1 and y = 2 in x + y *)
  | EIf of expression * expression * expression (** if true then 1 else 0 *)
  | EMatchWith of expression * (pattern * expression) * (pattern * expression) list
  (** match x with _ -> x *)

and declaration =
  | DDeclaration of id * pattern list * expression (** let add x y = x + y *)
  | DRecursiveDeclaration of id * pattern list * expression
  (** let rec factorial n = n * factorial (n - 1) *)

(* Smart constructors for literals *)
let lint x = LInt x
let lstring x = LString x

let lstring_with_quotes x =
  let substring_len = String.length x - 2 in
  String.sub x 1 substring_len |> lstring
;;

let lchar x = LChar x
let lbool x = LBool x

(* Smart constructors for expressions *)
let eliteral x = ELiteral x
let eidentifier x = EIdentifier x

let etuple first_elem second_elem other_elems =
  ETuple (first_elem, second_elem, other_elems)
;;

let elist x = EList x
let efun first_arg other_args fun_body = EFun (first_arg, other_args, fun_body)

let ebinary_operation operator left_operand right_operand =
  EBinaryOperation (operator, left_operand, right_operand)
;;

let ddeclaration function_name variable_list expression =
  DDeclaration (function_name, variable_list, expression)
;;

let drecursivedeclaration function_name variable_list expression =
  DRecursiveDeclaration (function_name, variable_list, expression)
;;

let eif condition true_branch false_branch = EIf (condition, true_branch, false_branch)

let ematchwith matched_expression first_case other_cases =
  EMatchWith (matched_expression, first_case, other_cases)
;;

let eletin first_declaration other_declarations body =
  ELetIn (first_declaration, other_declarations, body)
;;

let eapplication function_expression operand_expression =
  EApplication (function_expression, operand_expression)
;;

let eunary_operation operation expression = EUnaryOperation (operation, expression)
let econstruct_list head tail = EConstructList (head, tail)

(* Smart constructors for binary operators *)
let badd _ = Add
let bsub _ = Sub
let bmul _ = Mul
let bdiv _ = Div
let beq _ = Eq
let bneq _ = NEq
let bgt _ = GT
let bgte _ = GTE
let blt _ = LT
let blte _ = LTE
let band _ = AND
let bor _ = OR
(* --------------------------------------- *)

(* Smart constructors for unary operators *)
let uminus _ = Minus
let unot _ = Not
(* -------------------------------------- *)

(* Smart constructors for patterns *)
let pliteral literal = PLiteral literal
let pwildcard _ = PWildcard

let ptuple first_pattern second_pattern other_patterns =
  PTuple (first_pattern, second_pattern, other_patterns)
;;

let plist pattern_list = PList pattern_list
let pconstruct_list head tail = PConstructList (head, tail)
let pidentifier id = PIdentifier id
