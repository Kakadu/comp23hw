(** Copyright 2023-2024, Grigory Aseev and Matvey Kalashnikov *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

(** The type of identifiers *)
type id = string [@@deriving eq, show { with_path = false }]

(** The type of binary operations *)
type bin_op =
  | Add (** + *)
  | Sub (** - *)
  | Mul (** * *)
  | Div (** / *)
  | Mod (** % *)
  | Less (** < *)
  | Leq (** <= *)
  | Gre (** > *)
  | Geq (** >= *)
  | Eq (** = *)
  | Neq (** <> *)
  | And (** && *)
  | Or (** || *)
[@@deriving show { with_path = false }]

(** The type of constants *)
type const =
  | CBool of bool (** true *)
  | CInt of int (** 777 *)
  | CUnit (** () *)
[@@deriving show { with_path = false }]

(** The type of patterns *)
type pattern =
  | PWild (** _ *)
  | PConst of const (** 1 *)
  | PVar of id (** abc *)
[@@deriving show { with_path = false }]

(** The type of expressions *)
type expr =
  | EConst of const (** 1 *)
  | EBinOp of bin_op * expr * expr (** 1 + 1 *)
  | EVar of id (**  abc *)
  | EIf of expr * expr * expr (** if x > 0 then x else 0 *)
  | EFun of pattern * expr (** fun x -> x * 2 *)
  | ELetIn of bool * id * expr * expr (** let [rec] f x = e in e' *)
  | EApp of expr * expr (** f x *)
[@@deriving show { with_path = false }]

(** The type of bindings *)
type binding = ELet of bool * id * expr (** let [rec] f x = e *)
[@@deriving show { with_path = false }]

(** The type of programs *)
type program = binding list [@@deriving show { with_path = false }]

(**  Const constructors *)
val constr_cint : int -> const

val constr_cbool : bool -> const

(**  Pattern constructors *)

val constr_pwild : string -> pattern
val constr_pconst : const -> pattern
val constr_pvar : id -> pattern

(**  Operation constructor *)
val constr_ebinop : bin_op -> expr -> expr -> expr

(**  Expr constructors *)
val constr_econst : const -> expr

val constr_evar : id -> expr
val constr_eif : expr -> expr -> expr -> expr
val constr_efun : pattern list -> expr -> expr
val constr_eletin : bool -> id -> expr -> expr -> expr
val constr_eapp : expr -> expr list -> expr

(**  Binding constructor *)
val constr_elet : bool -> id -> expr -> binding
