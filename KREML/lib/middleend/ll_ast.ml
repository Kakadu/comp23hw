(** Copyright 2023-2024, Anton Kraev and Polina Badreeva *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast

(** Simplified ast representation which is the result of lambda lifting stage.
 The difference with cc_expr is that there are no nested and anonymous functions *)

type ll_expr =
  | LLiteral of literal
  | LIdentifier of identifier
  | LUnaryOp of unary_op * ll_expr
  | LBinaryOp of binary_op * ll_expr * ll_expr
  | LApp of ll_expr * ll_expr
  | LIfThenElse of ll_expr * ll_expr * ll_expr
  | LLetIn of identifier * ll_expr * ll_expr
[@@deriving show { with_path = false }]

type ll_binding =
  | LVal of identifier * ll_expr
  | LFun of identifier * identifier list * ll_expr
[@@deriving show { with_path = false }]
