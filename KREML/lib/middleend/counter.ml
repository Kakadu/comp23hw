(** Copyright 2023-2024, Anton Kraev and Polina Badreeva *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

let count = ref 0

let gen_var base_name () =
  incr count;
  Printf.sprintf "%s_%d" base_name !count
;;

let reset () = count := 0
