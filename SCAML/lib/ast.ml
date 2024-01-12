(** Copyright 2023-2024, Grigory Aseev and Matvey Kalashnikov *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

type id = string [@@deriving eq, show { with_path = false }]

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

type const =
  | CBool of bool (** true *)
  | CInt of int (** 777 *)
  | CUnit (** () *)
[@@deriving show { with_path = false }]

type pattern =
  | PWild (** _ *)
  | PConst of const (** 1 *)
  | PVar of id (** abc *)
[@@deriving show { with_path = false }]

type expr =
  | EConst of const (** 1 *)
  | EBinOp of bin_op * expr * expr (** 1 + 1 *)
  | EVar of id (**  abc *)
  | EIf of expr * expr * expr (** if x > 0 then x else 0 *)
  | EFun of pattern * expr (** fun x -> x * 2 *)
  | ELetIn of bool * id * expr * expr (** let [rec] f x = e in e' *)
  | EApp of expr * expr (** f x *)
[@@deriving show { with_path = false }]

type binding = ELet of bool * id * expr (** let [rec] f x = e *)
[@@deriving show { with_path = false }]

(** e1 ;; e2 ;; ... ;; en;; *)
type program = binding list [@@deriving show { with_path = false }]

(**  Const constructors *)
let constr_cint n = CInt n

let constr_cbool b = CBool b

(**  Pattern constructors *)

let constr_pwild _ = PWild
let constr_pconst c = PConst c
let constr_pvar id = PVar id

(**  Operation constructor *)
let constr_ebinop binary_op expr1 expr2 = EBinOp (binary_op, expr1, expr2)

(**  Expr constructors *)
let constr_econst e = EConst e

let constr_evar id = EVar id
let constr_eif e1 e2 e3 = EIf (e1, e2, e3)
let constr_efun pl e = List.fold_right (fun p e -> EFun (p, e)) pl e
let constr_eletin b id e1 e2 = ELetIn (b, id, e1, e2)
let constr_eapp f args = List.fold_left (fun f arg -> EApp (f, arg)) f args

(**  Binding constructor *)
let constr_elet b id e = ELet (b, id, e)
