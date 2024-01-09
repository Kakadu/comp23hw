(** Copyright 2023-2024, Grigory Aseev and Matvey Kalashnikov *)

(** SPDX-License-Identifier: LGPL-3.0 *)

val codegen_program : RestrictedAst.bexpr list -> (Llvm.llvalue list, string) Result.t
