(** Copyright 2023-2024, Grigory Aseev and Matvey Kalashnikov *)

(** SPDX-License-Identifier: LGPL-3.0 *)

open Llast
open Format

(** Pretty-prints a low-level expression to a formatter *)
val pp_llexpr : formatter -> llexpr -> unit

(** Pretty-prints a low-level binding to a formatter *)
val pp_llbinding : formatter -> llbinding -> unit
