(** Copyright 2023-2024, Danila Pechenev, Ilya Dudnikov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast

val find_identifiers_pattern : pattern -> (id, Base.String.comparator_witness) Base.Set.t
val find_identifiers : expression -> (id, Base.String.comparator_witness) Base.Set.t
