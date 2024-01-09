(** Copyright 2023-2024, Rustam Shangareev and Danil Yevdokimov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

(** Types and utility functions for type-checking *)

(** Representation of a type variable number *)
type type_variable_number = int

(** Representation of an identifier *)
type identifier = string

(** Basic ground types in the language *)
type ground_type =
  | Int (** Integer type *)
  | Bool (** Boolean type *)

(** Representation of types in the language *)
type typ =
  | TVar of type_variable_number (** Type variable, e.g., 'a *)
  | TArr of typ * typ (** Function type, e.g., int -> bool *)
  | TGround of ground_type (** Ground types *)

(** Utility function to represent the int type *)
val int_typ : typ

(** Utility function to represent the bool type *)
val bool_typ : typ

(** Utility function to generate a type variable *)
val var_t : type_variable_number -> typ

(** Utility function to generate a function type *)
val arrow_t : typ -> typ -> typ

(** Type schemes, including set of type variables and the type *)
type scheme = (type_variable_number, Base.Int.comparator_witness) Base.Set.t * typ

(** Errors that can occur during type-checking *)
type error =
  [ `OccursCheck (** Occurs check fail *)
  | `NoVariable of identifier (** Using undefined variable *)
  | `UnificationFailed of typ * typ (** Unify of types failed *)
  | `Unreachable (** Bug in parser *)
  | `Not_function
  | `Matching_failed
  ]

(** Function to pretty-print types *)
val pp_type : Format.formatter -> typ -> unit

(** Function to print types *)
val print_typ : typ -> unit

(** Function to pretty-print type errors *)
val pp_error : Format.formatter -> error -> unit

(** Function to print type errors *)
val print_type_error : error -> unit
