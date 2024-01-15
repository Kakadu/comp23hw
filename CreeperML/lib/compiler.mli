(** Copyright 2023-2024, Arthur Alekseev and Starcev Matvey *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

module Compiler : sig
  val compile : string -> unit
  val dmp_code : string -> unit
end
