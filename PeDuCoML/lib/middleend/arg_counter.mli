(** Copyright 2023-2024, Danila Pechenev, Ilya Dudnikov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

val gather_args_numbers
  :  Anf.global_scope_function list
  -> (string, (Anf.unique_id, int) Base.Map.Poly.t) Base.Map.Poly.t
