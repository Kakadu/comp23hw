(** Copyright 2023-2024, Anton Kraev and Polina Badreeva *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type identifier = string [@@deriving show { with_path = false }]

type literal =
  | LInt of int
  | LBool of bool
[@@deriving show { with_path = false }]

type binary_op =
  | Add (* + *)
  | Sub (* - *)
  | Mult (* * *)
  | Div (* / *)
  | Eq (* = *)
  | Lt (* < *)
  | LtOrEq (* <= *)
  | Gt (* > *)
  | GtOrEq (* >= *)
  | And (* and *)
  | Or (* or *)
[@@deriving show { with_path = false }]

type unary_op =
  | Neg (* ~ *)
  | Not (* not *)
[@@deriving show { with_path = false }]

type expr =
  | ELiteral of literal (* 55 *)
  | EIdentifier of identifier (* varname *)
  | EUnaryOp of unary_op * expr (* ~10 *)
  | EBinaryOp of binary_op * expr * expr (* 7 + 8 *)
  | EApp of expr * expr (* f x *)
  | EAbs of identifier * expr (* fn x => x + 1 *)
  | EIfThenElse of expr * expr * expr (* if true then 1 else 2 *)
  | ELetIn of binding list * expr (* let fun f x = x val y = 1 in f 1 * y *)

and binding =
  | BVal of identifier * expr (* val x = 88 *)
  | BFun of identifier * identifier list * expr (* fun sqr x = x * x *)
[@@deriving show { with_path = false }]

(* smart constructors *)

(* expressions *)
let e_literal x = ELiteral x
let e_identifier x = EIdentifier x
let e_unary_op op x = EUnaryOp (op, x)
let e_app x1 x2 = EApp (x1, x2)
let e_abs arg body = EAbs (arg, body)
let e_if_then_else cond if_true if_false = EIfThenElse (cond, if_true, if_false)
let e_binary_op op left right = EBinaryOp (op, left, right)
let e_let_in bindings body = ELetIn (bindings, body)

(* bindings *)
let b_val value_id expression = BVal (value_id, expression)
let b_fun fun_id args body = BFun (fun_id, args, body)

(* binary operations *)
let badd _ = Add
let bsub _ = Sub
let bmul _ = Mult
let bdiv _ = Div
let beq _ = Eq
let blt _ = Lt
let blte _ = LtOrEq
let bgt _ = Gt
let bgte _ = GtOrEq
let band _ = And
let bor _ = Or

(* unary operations *)
let uneg _ = Neg
let unot _ = Not

(* literals *)
let lint x = LInt x
let lbool x = LBool x
