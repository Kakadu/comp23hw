(** Copyright 2023-2024, Danila Pechenev, Ilya Dudnikov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type 'a t

include Base.Monad.Infix with type 'a t := 'a t

val fresh : int t
val bind : 'a t -> ('a -> 'b t) -> 'b t
val return : 'a -> 'a t
val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
val run : 'a t -> 'a
