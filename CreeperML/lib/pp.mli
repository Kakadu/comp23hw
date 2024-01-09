(** Copyright 2023-2024, Arthur Alekseev and Starcev Matvey *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

module PrettyPrinter : sig
  open Closure.ClosureAst
  open Anf.AnfTypeAst
  open Type_ast.TypeAst
  open Indexed_ast.IndexedTypeAst
  (* open Asm.Asm *)

  val print_cf_program : bool -> cf_typ_program -> string
  val print_anf_program : bool -> anf_program -> string
  val print_typ_program : bool -> ty typ_program -> string
  val print_index_program : bool -> index_program -> string
  (* val print_asm_program : Asm.Asm.asm_program -> string *)
end
