(** Copyright 2023-2024, Arthur Alekseev and Starcev Matvey *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

module Result : sig
  type 'a t = ('a, string) Result.t

  val return : 'a -> 'a t
  val error : string -> 'a t
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  val ( *> ) : 'a t -> 'b t -> 'b t
  val ( >>| ) : 'a t -> ('a -> 'b) -> 'b t
  val ( |<< ) : ('a -> 'b) -> 'a t -> 'b t
  val monadic_map : 'a list -> ('a -> 'b t) -> 'b list t
  val monadic_mapi : 'a list -> (int -> 'a -> 'b t) -> 'b list t
  val monadic_fold : ('acc -> 'a -> 'acc t) -> 'acc -> 'a list -> 'acc t
end
