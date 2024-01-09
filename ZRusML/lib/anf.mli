(** Copyright 2023-2024, Rustam Shangareev and Danil Yevdokimov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

open Ast

type immexpr =
  | ImmInt of int
  | ImmBool of bool
  | ImmIdentifier of string

type cexpr =
  | CImmExpr of immexpr
  | CUnaryOp of un_op * immexpr
  | CBinaryOp of bin_op * immexpr * immexpr
  | CApp of immexpr * immexpr
  | CIf of immexpr * immexpr * immexpr

type aexpr =
  | ALet of string * cexpr * aexpr
  | ACExpr of cexpr

type abinding = AVal of string * aexpr

val anf_func : (unit -> id) -> exp -> (immexpr -> aexpr) -> aexpr
val anf_program : prog -> abinding list
