(** Copyright 2023-2024, Grigory Aseev and Matvey Kalashnikov *)

(** SPDX-License-Identifier: LGPL-3.0 *)

open RestrictedAst
open Format
open Ast

(* A function to print an expression of immediate type *)
val pp_immexpr : formatter -> immexpr -> unit

(* A function to print a binary operation *)
val pp_binop : formatter -> bin_op -> unit

(* A function to print an expression of composite type *)
val pp_cexpr : formatter -> cexpr -> unit

(* A function to print an expression of pattern type *)
val pp_pexpr : formatter -> pexpr -> unit

(* A function to print an expression of abstract type *)
val pp_aexpr : formatter -> aexpr -> unit

(* A function to print a list of values with a given separator *)
val pp_list
  :  formatter
  -> (formatter -> 'a -> unit)
  -> (unit, Format.formatter, unit) format
  -> 'a list
  -> unit

(* A function to print an expression of binding type *)
val pp_bexpr : formatter -> bexpr -> unit

(* A function to print an expression of program type *)
val pp_prexpr : formatter -> bexpr list -> unit
