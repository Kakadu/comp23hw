(** Copyright 2023-2024, Arthur Alekseev and Starcev Matvey *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

module Std : sig
  val typeenv : (string * Type_ast.InferType.ty Type_ast.InferType.lvls) list
  val operators : (string, Type_ast.TypeAst.ty) Type_ast.TypeAst.typed list
end
