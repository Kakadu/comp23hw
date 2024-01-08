(** Copyright 2023-2024, Ilya Pankratov, Maxim Drumov *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

(** Counter *)
module Counter : sig
  val genid : string -> string
  val reset : int -> unit
end
