(** Copyright 2023-2024, Grigory Aseev and Matvey Kalashnikov *)

(** SPDX-License-Identifier: LGPL-3.0 *)

(* The code is a modified version of the inferencer taken from here
   https://gitlab.com/Kakadu/fp2020course-materials/-/blob/master/code/miniml*)

open Format

type binder = int [@@deriving show { with_path = false }]

module VarSet : sig
  include module type of Stdlib.Set.Make (Int)

  val pp : formatter -> t -> unit
end

type binder_set = VarSet.t [@@deriving show { with_path = false }]

type ty =
  | TBool
  | TInt
  | TUnit
  | TVar of binder
  | TArrow of ty * ty
[@@deriving show { with_path = false }]

type error =
  [ `Occurs_check
  | `No_variable of string
  | `Unification_failed of ty * ty
  ]

type scheme = S of binder_set * ty

val arrow : ty -> ty -> ty
val int_typ : ty
val bool_typ : ty
val unit_typ : ty
val v : binder -> ty
val pp_typ : formatter -> ty -> unit
val pp_scheme : formatter -> scheme -> unit
val print_typ : ty -> unit
val pp_error : formatter -> error -> unit
val print_typ_err : error -> unit
