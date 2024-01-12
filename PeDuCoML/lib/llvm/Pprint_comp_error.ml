(** Copyright 2023-2024, Danila Pechenev, Ilya Dudnikov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Llvm_comp
open Pprintanf

let pp_error fmt = function
  | UnboundVariable var -> Format.fprintf fmt "Error: unbound variable: %a" pp_id var
;;
