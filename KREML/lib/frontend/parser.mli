(** Copyright 2023-2024, Anton Kraev and Polina Badreeva *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

val parse : string -> (Ast.binding list, string) result
val parse_optimistically : string -> Ast.binding list
val parse_error : string -> bool
