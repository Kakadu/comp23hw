(** Copyright 2023-2024, Danila Pechenev, Ilya Dudnikov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type 'a t = int -> 'a * int

let fresh last = last, last + 1

let ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t =
  fun m f state ->
  let value, st = m state in
  (f value) st
;;

let bind m f = m >>= f
let return value st = value, st
let ( let* ) = ( >>= )
let run monad = fst @@ monad 0

let ( >>| ) : 'a t -> ('a -> 'b) -> 'b t =
  fun m f state ->
  let value, st = m state in
  f value, st
;;
