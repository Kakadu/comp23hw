(** Copyright 2023-2024, Danila Pechenev, Ilya Dudnikov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type riscv_error

val codegen : Anf.global_scope_function list -> (unit, riscv_error) Result.t
