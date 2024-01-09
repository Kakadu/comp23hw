(** Copyright 2023-2024, Danila Pechenev, Ilya Dudnikov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Typing

let no_arg_scheme = Base.Set.empty (module Base.Int), TGround Int
let one_arg_scheme = Base.Set.empty (module Base.Int), TArr (TGround Int, TGround Int)

let two_args_scheme =
  Base.Set.empty (module Base.Int), TArr (TGround Int, TArr (TGround Int, TGround Int))
;;

let runtime =
  [ "peducoml_alloc_closure", two_args_scheme
  ; "peducoml_apply", two_args_scheme
  ; "peducoml_apply0", one_arg_scheme
  ; "peducoml_alloc_list", no_arg_scheme
  ; "peducoml_add_to_list", two_args_scheme
  ; "peducoml_list_field", two_args_scheme
  ; "peducoml_tail", one_arg_scheme
  ; "peducoml_list_length", one_arg_scheme
  ; "peducoml_alloc_tuple", one_arg_scheme
  ; "peducoml_divide", two_args_scheme
  ; "peducoml_fill_tuple", two_args_scheme
  ; "peducoml_tuple_field", two_args_scheme
  ]
;;
