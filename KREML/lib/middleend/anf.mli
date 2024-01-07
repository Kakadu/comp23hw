(** Copyright 2023-2024, Anton Kraev and Polina Badreeva *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type immexpr =
  | ImmInt of int
  | ImmBool of bool
  | ImmIdentifier of string

type cexpr =
  | CImmExpr of immexpr
  | CUnaryOp of Ast.unary_op * immexpr
  | CBinaryOp of Ast.binary_op * immexpr * immexpr
  | CApp of immexpr * immexpr
  | CIfThenElse of immexpr * aexpr * aexpr

and aexpr =
  | ALet of string * cexpr * aexpr
  | ACExpr of cexpr

type abinding = AFun of Ast.identifier * Ast.identifier list * aexpr

val show_abinding : abinding -> string
