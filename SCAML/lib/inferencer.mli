(** Copyright 2023-2024, Grigory Aseev and Matvey Kalashnikov *)

(** SPDX-License-Identifier: LGPL-3.0 *)

(* The code is a modified version of the inferencer taken from here
   https://gitlab.com/Kakadu/fp2020course-materials/-/blob/master/code/miniml*)

open Ast
open Ty

val run_prog_inference : binding list -> ((id * ty) list, error) result
