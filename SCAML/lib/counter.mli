(** Copyright 2023-2024, Grigory Aseev and Matvey Kalashnikov *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

module type MONAD = sig
  type 'a t

  val return : 'a -> 'a t
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
end

module type STATE = sig
  type state

  include MONAD

  module Syntax : sig
    val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  end

  val get : state t
  val put : state -> unit t
  val runState : 'a t -> init:state -> state * 'a
end

module State (S : sig
    type t
  end) : STATE with type state = S.t

module IState : STATE with type state = int

(** [fresh_name_int] is a stateful computation that returns a fresh integer and increments the state. *)
val fresh_name_int : int IState.t
