(** Copyright 2023-2024, Danila Pechenev, Ilya Dudnikov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Lambda_lift
open Format

val pp_cfexpr : formatter -> cf_expr -> unit
val pp_cflet : formatter -> cf_let -> unit
val pp_declaration : formatter -> cf_decl -> unit
