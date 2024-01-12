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
val lint : int -> literal
val lstring : string -> literal
val lstring_with_quotes : string -> literal
val lchar : char -> literal
val lbool : bool -> literal

(* Smart constructors for expressions *)

val eliteral : literal -> expression
val eidentifier : id -> expression
val etuple : expression -> expression -> expression list -> expression
val elist : expression list -> expression
val efun : pattern -> pattern list -> expression -> expression
val ebinary_operation : binary_operator -> expression -> expression -> expression
val eif : expression -> expression -> expression -> expression

val ematchwith
  :  expression
  -> pattern * expression
  -> (pattern * expression) list
  -> expression

val eletin : declaration -> declaration list -> expression -> expression
val eapplication : expression -> expression -> expression
val eunary_operation : unary_operator -> expression -> expression
val econstruct_list : expression -> expression -> expression
val ddeclaration : id -> pattern list -> expression -> declaration
val drecursivedeclaration : id -> pattern list -> expression -> declaration

(* Smart constructors for binary operators *)
val badd : 'a -> binary_operator
val bsub : 'a -> binary_operator
val bmul : 'a -> binary_operator
val bdiv : 'a -> binary_operator
val beq : 'a -> binary_operator
val bneq : 'a -> binary_operator
val bgt : 'a -> binary_operator
val bgte : 'a -> binary_operator
val blt : 'a -> binary_operator
val blte : 'a -> binary_operator
val band : 'a -> binary_operator
val bor : 'a -> binary_operator
(* --------------------------------------- *)

(* Smart constructors for unary operators *)
val uminus : 'a -> unary_operator
val unot : 'a -> unary_operator
(* -------------------------------------- *)

(* Smart constructors for patterns *)

val pliteral : literal -> pattern
val pwildcard : 'a -> pattern
val ptuple : pattern -> pattern -> pattern list -> pattern
val plist : pattern list -> pattern
val pconstruct_list : pattern -> pattern -> pattern
val pidentifier : id -> pattern
