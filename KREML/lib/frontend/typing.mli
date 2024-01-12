(** Copyright 2023-2024, Anton Kraev and Polina Badreeva *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type ground_type =
  | Int
  | Bool

val pp_ground_type : Format.formatter -> ground_type -> unit
val show_ground_type : ground_type -> string

type typ =
  | TVar of int
  | TEqualityVar of int
  | TArr of typ * typ
  | TGround of ground_type
  | TUnit

val pp_typ : Format.formatter -> typ -> unit
val show_typ : typ -> string
val int_t : typ
val bool_t : typ
val unit_t : typ
val var_t : int -> typ
val var_eq_t : int -> typ
val arrow_t : typ -> typ -> typ

type scheme = (int, Base.Int.comparator_witness) Base.Set.t * typ

type error =
  [ `NoVariable of string
  | `OccursCheck
  | `UnificationFailed of typ * typ
  ]

val pp_type : Format.formatter -> typ -> unit
val pp_error : Format.formatter -> error -> unit
val print_type : typ -> unit
val print_type_error : error -> unit
