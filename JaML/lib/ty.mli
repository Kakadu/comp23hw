(** Copyright 2023-2024, Ilya Pankratov, Maxim Drumov *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

(** Ground types *)
type prim =
  | Int (** Integer type *)
  | Bool (** Boolean type *)
[@@deriving eq]

val show_prim : prim -> string
val pp_prim : Format.formatter -> prim -> unit

(** Types for expession *)
type ty =
  | Tyvar of int (** Represent polymorphic type *)
  | Prim of prim (** Ground types *)
  | Arrow of ty * ty (** Type for function *)
[@@deriving show { with_path = false }]

val tyint : ty
val tybool : ty
val arrow : ty -> ty -> ty
val var_typ : int -> ty
