(** Copyright 2023-2024, Arthur Alekseev and Starcev Matvey *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

module Codegen : sig
  val top_lvl : Anf.AnfTypeAst.anf_binding list -> unit Monad.Result.t
  val dmp_code : string -> unit
  val compile : Anf.AnfTypeAst.anf_binding list -> string -> unit
end
