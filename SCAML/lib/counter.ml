(** Copyright 2023-2024, Grigory Aseev and Matvey Kalashnikov *)

(** SPDX-License-Identifier: LGPL-3.0 *)

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
  end) : STATE with type state = S.t = struct
  type state = S.t
  type 'a t = state -> state * 'a

  let return v s = s, v

  let ( >>= ) m k s =
    let s', a = m s in
    k a s'
  ;;

  module Syntax = struct
    let ( let* ) x f = x >>= f
  end

  let get s = s, s
  let put s' _ = s', ()
  let runState m ~init = m init
end

module IState = State (struct
    type t = int
  end)

let fresh_name_int : int IState.t =
  let open IState in
  get >>= fun i -> put (i + 1) >>= fun () -> return i
;;
