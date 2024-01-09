(** Copyright 2023-2024, Grigory Aseev and Matvey Kalashnikov *)

(** SPDX-License-Identifier: LGPL-3.0 *)

open Ast
open Llast

(* A function that runs the lifting function with an initial state *)
val run_ll : binding list -> llbinding list
