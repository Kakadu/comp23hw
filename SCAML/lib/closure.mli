(** Copyright 2023-2024, Grigory Aseev and Matvey Kalashnikov *)

(** SPDX-License-Identifier: LGPL-3.0 *)

open Ast

(** [prog_conversion program] returns a program that is equivalent to [program] but with closure conversion applied to each declaration. *)
val prog_conversion : binding list -> binding list