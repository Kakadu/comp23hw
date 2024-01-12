(** Copyright 2023-2024, Danila Pechenev, Ilya Dudnikov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Format

val pp_literal : formatter -> literal -> unit
val pp_binary_operator : formatter -> binary_operator -> unit
val pp_unary_operator : formatter -> unary_operator -> unit
val pp_pattern : formatter -> pattern -> unit
val pp_expression : formatter -> expression -> unit
val pp_declaration : formatter -> declaration -> unit
