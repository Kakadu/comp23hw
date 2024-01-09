(** Copyright 2023-2024, Rustam Shangareev and Danil Yevdokimov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

(** [inference code] performs type inference on the given [code].
    If the code is correctly parsed and no type errors are found,
    it prints the inferred types for each declaration.
    If a type error is detected, it prints an error message indicating the location of the error.
    If the code cannot be parsed, it prints "Parse error".
    @param code The input code as a string. *)
val inference : Format.formatter -> string -> unit
