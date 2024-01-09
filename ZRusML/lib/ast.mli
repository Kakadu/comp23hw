(** Copyright 2023-2024, Rustam Shangareev and Danil Yevdokimov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

(** Abstract Syntax Tree representation for the language *)

(** Identifier type, usually representing variable names *)
type id = string [@@deriving show { with_path = false }]

(** Binary operations available in the language *)
type bin_op =
  | And (** Logical and *)
  | Or (** Logical or *)
  | Less (** Less than *)
  | Leq (** Less or equal *)
  | Gre (** Greater than *)
  | Geq (** Greater or equal *)
  | Eq (** Equality *)
  | Neq (** Not equal *)
  | Add (** Addition *)
  | Sub (** Subtraction *)
  | Mul (** Multiplication *)
  | Div (** Division *)

(** Unary operations available in the language *)
and un_op =
  | Not (** Logical not *)
  | Minus (** Unary minus *)

(** Binding type for let expressions and function arguments *)
and binding = bool * pt * exp

(** Case type for pattern matching *)
and case = pt * exp

(** Declarations available in the language *)
and decl = DLet of binding (** let declaration *)

(** Program type representing a list of declarations *)
and prog = decl list

(** Constants available in the language *)
and const =
  | CInt of int (** Integer constant *)
  | CBool of bool (** Boolean constant *)

(** Expressions in the language *)
and exp =
  | EConst of const (** Constants *)
  | EUnOp of un_op * exp (** Unary operations *)
  | EVar of id (** Variables *)
  | ELet of binding list * exp (** Let expressions *)
  | EFun of pt * exp (** Function definitions *)
  | EIf of exp * exp * exp (** Conditional expressions *)
  | EBinOp of bin_op * exp * exp (** Binary operations *)
  | EApp of exp * exp (** Function application *)

(** Patterns available in the language for pattern matching *)
and pt =
  | PtWild (** Wildcard pattern *)
  | PtVar of id (** Variable pattern *)
  | PtConst of const (** Constant pattern *)
[@@deriving show { with_path = false }]
