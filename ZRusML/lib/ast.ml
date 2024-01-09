(** Copyright 2023-2024, Rustam Shangareev and Danil Yevdokimov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

type id = string [@@deriving show { with_path = false }]

type bin_op =
  | And (**  and *)
  | Or (**  or *)
  | Less (**  < *)
  | Leq (**  <= *)
  | Gre (**  > *)
  | Geq (**  >= *)
  | Eq (** = *)
  | Neq (**  <> *)
  | Add (**  +   *)
  | Sub (**  -   *)
  | Mul (**  * *)
  | Div (**  / *)

and un_op =
  | Not (**  not *)
  | Minus (** - *)

and binding = bool * pt * exp
and case = pt * exp
and decl = DLet of binding (**  val y = 256 *)
and prog = decl list

and const =
  | CInt of int
  | CBool of bool

and exp =
  | EConst of const (**    false *)
  | EUnOp of un_op * exp (**    not x, -x, !x *)
  | EVar of id (**    x *)
  | ELet of binding list * exp (**    let x = 256 in 512 *)
  | EFun of pt * exp (**   fn x -> x *)
  | EIf of exp * exp * exp (**    if predicate then x else y *)
  | EBinOp of bin_op * exp * exp (**    25 / (7 + -2) *)
  | EApp of exp * exp (**    fold a list init *)

and pt =
  | PtWild (**  _ *)
  | PtVar of id (**  xyz *)
  | PtConst of const (**  256 *)
[@@deriving show { with_path = false }]
