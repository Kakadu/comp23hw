(** Copyright 2023-2024, Danila Pechenev, Ilya Dudnikov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Match_elim

type unique_id =
  | AnfId of int
  | GlobalScopeId of string

type imm_expr =
  | ImmInt of int
  | ImmString of string
  | ImmChar of char
  | ImmBool of bool
  | ImmId of unique_id

type cexpr =
  | CBinaryOperation of binary_operator * imm_expr * imm_expr
  | CUnaryOperation of unary_operator * imm_expr
  | CApplication of imm_expr * imm_expr
  | CList of imm_expr list
  | CTuple of imm_expr list
  | CIf of imm_expr * imm_expr * imm_expr
  | CConstructList of imm_expr * imm_expr
  | CImm of imm_expr

type aexpr =
  | ALet of unique_id * cexpr * aexpr
  | ACExpr of cexpr

type global_scope_function = string * imm_expr list * aexpr

val run_anf_conversion : match_free_decl list -> global_scope_function list
