(** Copyright 2023-2024, Danila Pechenev, Ilya Dudnikov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type rv_type =
  | Imm
  | Register
  | Offset
  | Binding

type rv_value

val lookup_function : string -> rv_value option
val lookup_function_exn : string -> rv_value
val build_load : rv_value -> rv_value
val build_store : rv_value -> rv_value
val const_int : int -> rv_value
val declare_function : string -> 'a list -> rv_value * ('a * rv_value) list
val build_call : rv_value -> rv_value list -> rv_value
val build_add : rv_value -> rv_value -> rv_value
val build_sub : rv_value -> rv_value -> rv_value
val build_mul : rv_value -> rv_value -> rv_value
val build_div : rv_value -> rv_value -> rv_value
val build_and : rv_value -> rv_value -> rv_value
val build_or : rv_value -> rv_value -> rv_value
val build_eq : rv_value -> rv_value -> rv_value
val build_neq : rv_value -> rv_value -> rv_value
val build_lt : rv_value -> rv_value -> rv_value
val build_gte : rv_value -> rv_value -> rv_value
val build_lte : rv_value -> rv_value -> rv_value
val build_gt : rv_value -> rv_value -> rv_value
val build_ret : string -> rv_value -> unit
val params : rv_value -> int option
val build_neg : rv_value -> rv_value
val build_not : rv_value -> rv_value
val get_basicblock : string -> string
val build_basicblock : string -> unit
val build_beq : rv_value -> string -> unit
val build_jump : string -> unit
val build_alloca : 'a -> rv_value
val build_store_dst : rv_value -> rv_value -> rv_value
val type_of : rv_value -> rv_type
