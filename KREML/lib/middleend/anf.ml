(** Copyright 2023-2024, Anton Kraev and Polina Badreeva *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast

type immexpr =
  | ImmInt of int
  | ImmBool of bool
  | ImmIdentifier of string
[@@deriving show { with_path = false }]

type cexpr =
  | CImmExpr of immexpr
  | CUnaryOp of unary_op * immexpr
  | CBinaryOp of binary_op * immexpr * immexpr
  | CApp of immexpr * immexpr
  | CIfThenElse of immexpr * aexpr * aexpr
[@@deriving show { with_path = false }]

and aexpr =
  | ALet of string * cexpr * aexpr
  | ACExpr of cexpr
[@@deriving show { with_path = false }]

type abinding = AFun of identifier * identifier list * aexpr
[@@deriving show { with_path = false }]
