(** Copyright 2023-2024, Danila Pechenev, Ilya Dudnikov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Anf
open Format

val pp_id : formatter -> unique_id -> unit
val pp_immexpr : formatter -> imm_expr -> unit
val pp_cexpr : formatter -> cexpr -> unit
val pp_aexpr : formatter -> aexpr -> unit
val pp_global_scope_function : formatter -> tag * unique_id list * aexpr -> unit
