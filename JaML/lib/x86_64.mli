(** Copyright 2024, Ilya Pankratov, Maxim Drumov *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

val assembly
  :  Anf.anfexpr list
  -> (Monads.CompilerMonad.command list, Monads.CompilerMonad.error) result
