(** Copyright 2023-2024, Danila Pechenev, Ilya Dudnikov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Format

type type_variable_number = int
type identifier = string

type ground_type =
  | Int
  | String
  | Char
  | Bool
[@@deriving eq, show { with_path = false }]

type typ =
  | TVar of type_variable_number (** 'a *)
  | TArr of typ * typ (** string -> int *)
  | TTuple of typ list (** int * int *)
  | TList of typ (** 'a list *)
  | TGround of ground_type (** int *)

val pp_type : formatter -> typ -> unit
val print_typ : typ -> unit

type scheme = (type_variable_number, Base.Int.comparator_witness) Base.Set.t * typ

type error =
  [ `Occurs_check
  | `NoVariable of identifier
  | `NoConstructor of identifier
  | `UnificationFailed of typ * typ
  ]

val pp_error : formatter -> error -> unit
val print_type_error : error -> unit
