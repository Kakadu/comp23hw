(** Copyright 2023-2024, Ilya Pankratov, Maxim Drumov *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

(** Ground types *)
type prim =
  | Int (** Integer type *)
  | Bool (** Boolean type *)
[@@deriving eq]

let show_prim = function
  | Int -> "int"
  | Bool -> "bool"
;;

let pp_prim ppf prim = Stdlib.Format.fprintf ppf "%s" (show_prim prim)

(** Types for expession *)
type ty =
  | Tyvar of int (** Represent polymorphic type *)
  | Prim of prim (** Ground types *)
  | Arrow of ty * ty (** Type for function *)
[@@deriving show { with_path = false }]

(* Constructors for ground types *)
let tyint = Prim Int
let tybool = Prim Bool

(* Constructors *)
let arrow l r = Arrow (l, r)
let var_typ x = Tyvar x
