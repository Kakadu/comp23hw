(** Copyright 2023-2024, Arthur Alekseev and Starcev Matvey *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type state = { next : int }
type 'a t = state -> 'a * state

val bind : 'a t -> ('a -> 'b t) -> 'b t
val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
val return : 'a -> state -> 'a * state
val new_var : string -> state -> string * state
val ( >>| ) : 'a t -> ('a -> 'b) -> 'b t
val ( |<< ) : ('a -> 'b) -> 'a t -> 'b t
val monadic_map : 'a list -> ('a -> 'b t) -> 'b list t
val monadic_fold : ('acc -> 'a -> 'acc t) -> 'acc -> 'a list -> 'acc t
val run : 'a t -> 'a
val monadic_mapi : 'a list -> (int -> 'a -> 'b t) -> 'b list t
