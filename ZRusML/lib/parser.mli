(** Copyright 2023-2024, Rustam Shangareev and Danil Yevdokimov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

(* This module provides a parser for the language. *)

open Ast
open Base
open Angstrom

(** [parse p s] applies the parser [p] on string [s]. *)
val parse : 'a t -> string -> ('a, string) result

(** [prog] is the main parser for the entire program. *)
val prog : prog t
