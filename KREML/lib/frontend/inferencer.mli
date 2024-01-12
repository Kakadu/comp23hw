(** Copyright 2023-2024, Anton Kraev and Polina Badreeva *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

val infer : Ast.binding list -> unit
val parse_and_infer : string -> unit
val run_inference : Ast.binding list -> (Typing.typ, Typing.error) result
