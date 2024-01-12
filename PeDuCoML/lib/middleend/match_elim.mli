(** Copyright 2023-2024, Danila Pechenev, Ilya Dudnikov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Lambda_lift

type match_free_expr =
  | MFLiteral of literal
  | MFBinaryOperation of binary_operator * match_free_expr * match_free_expr
  | MFUnaryOperation of unary_operator * match_free_expr
  | MFApplication of match_free_expr * match_free_expr
  | MFIdentifier of id
  | MFList of match_free_expr list
  | MFConstructList of match_free_expr * match_free_expr
  | MFLetIn of match_free_let * match_free_let list * match_free_expr
  | MFTuple of match_free_expr * match_free_expr * match_free_expr list
  | MFIf of match_free_expr * match_free_expr * match_free_expr

and match_free_let = id * match_free_expr

type match_free_decl =
  | MFDeclaration of id * id list * match_free_expr
  | MFRecursiveDeclaration of id * id list * match_free_expr

val elim_match : cf_decl list -> match_free_decl list
