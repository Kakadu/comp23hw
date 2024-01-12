(** Copyright 2023-2024, Danila Pechenev, Ilya Dudnikov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type llvm_error = UnboundVariable of Anf.unique_id

val codegen : Anf.global_scope_function list -> (Llvm.llvalue list, llvm_error) Result.t
