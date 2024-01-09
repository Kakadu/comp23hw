(** Copyright 2023-2024, Rustam Shangareev and Danil Yevdokimov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

(** The main type inference module for the language. *)

(* Dependencies *)
open Base
open Ast
open Typing

(* The main monadic type for our inference process. *)
module R : sig
  type 'a t

  (** Bind function for our monadic type. *)
  val bind : 'a t -> f:('a -> 'b t) -> 'b t

  (** Return function for our monadic type. *)
  val return : 'a -> 'a t

  (** Function to fail with a specific error. *)
  val fail : error -> 'a t

  (** Some monadic infix operators for syntactic sugar. *)
  include Monad.Infix with type 'a t := 'a t

  module Syntax : sig
    val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  end

  module RMap : sig
    (** Fold left function that works with our custom monad. *)
    val fold_left
      :  (int, 'a, Int.comparator_witness) Map.t
      -> init:'b t
      -> f:(int -> 'a -> 'b -> 'b t)
      -> 'b t
  end

  (** Produces a fresh integer. *)
  val fresh : int t

  (** Runs the monad, producing a result. *)
  val run : 'a t -> ('a, error) Result.t
end

type fresh = int

module Type : sig
  type t = typ

  (** Checks if a type variable occurs in a type. *)
  val occurs_in : type_variable_number -> t -> bool

  (** Extracts type variables from a type. *)
  val unpack_vars : t -> (type_variable_number, Int.comparator_witness) Set.t
end

module Subst : sig
  type t

  val empty : t

  (** Creates a singleton substitution. *)
  val singleton : fresh -> typ -> t R.t

  (** Find functions for substitutions. *)
  val find_exn : fresh -> t -> typ

  val find : fresh -> t -> typ option

  (** Applies a substitution to a type. *)
  val apply : t -> typ -> typ

  (** Attempts to unify two types, producing a substitution. *)
  val unify : typ -> typ -> t R.t

  val add : t -> fresh -> typ -> t
  val compose : t -> t -> t R.t
  val compose_all : t list -> t R.t

  (** Removes a substitution. *)
  val remove : t -> fresh -> t
end

module VarSet : sig
  val fold : ('a -> 'b -> 'a R.t) -> 'a R.t -> ('b, 'c) Base.Set.t -> 'a R.t
end

module Scheme : sig
  type t = scheme

  (** Checks if a type variable occurs in a scheme. *)
  val occurs_in : type_variable_number -> t -> bool

  (** Extracts type variables from a scheme. *)
  val unpack_vars : t -> (type_variable_number, Int.comparator_witness) Set.t

  (** Applies a substitution to a scheme. *)
  val apply : Subst.t -> t -> t
end

module TypeEnv : sig
  type t = (identifier, scheme, String.comparator_witness) Map.t

  val extend : t -> identifier -> scheme -> t
  val empty : t

  (** Extracts type variables from an environment. *)
  val unpack_vars : t -> (type_variable_number, Int.comparator_witness) Set.t

  (** Applies a substitution to an environment. *)
  val apply : Subst.t -> t -> t

  val find_exn : 'a -> ('a, equal:(string -> string -> bool) -> 'b, 'c) Base.Map.t -> 'b
end

(** The main inference function for expressions. *)
val infer : TypeEnv.t -> exp -> (Subst.t * Type.t * TypeEnv.t) R.t

val run_inference
  :  Ast.decl
  -> TypeEnv.t
  -> ( (Typing.identifier, Typing.scheme, Base.String.comparator_witness) Base.Map.t
       * Typing.typ
       , Typing.error )
       Base.Result.t
