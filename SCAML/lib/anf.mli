(** Copyright 2023-2024, Grigory Aseev and Matvey Kalashnikov *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open Llast
open RestrictedAst

(* Converts a list of linear lambda bindings to a list of annotated bindings *)
val anf_program : llbinding list -> bexpr list