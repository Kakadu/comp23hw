(** Copyright 2023-2024, Danila Pechenev, Ilya Dudnikov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast

type cf_expr =
  | CFLiteral of literal
  | CFBinaryOperation of binary_operator * cf_expr * cf_expr
  | CFUnaryOperation of unary_operator * cf_expr
  | CFApplication of cf_expr * cf_expr
  | CFIdentifier of id
  | CFList of cf_expr list
  | CFConstructList of cf_expr * cf_expr
  | CFLetIn of cf_let * cf_let list * cf_expr
  | CFTuple of cf_expr * cf_expr * cf_expr list
  | CFMatchWith of cf_expr * (pattern * cf_expr) * (pattern * cf_expr) list
  | CFIf of cf_expr * cf_expr * cf_expr

and cf_let = id * cf_expr

type cf_decl =
  | CFDeclaration of id * id list * cf_expr
  | CFRecursiveDeclaration of id * id list * cf_expr

val run_lambda_lifting : declaration list -> cf_decl list
