(** Copyright 2023-2024, Danila Pechenev, Ilya Dudnikov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Typing

let print_int_scheme = Base.Set.empty (module Base.Int), TArr (TGround Int, TGround Int)
let print_char_scheme = Base.Set.empty (module Base.Int), TArr (TGround Char, TGround Int)
let print_bool_scheme = Base.Set.empty (module Base.Int), TArr (TGround Bool, TGround Int)
let print_new_line_scheme = Base.Set.empty (module Base.Int), TGround Int

let print_list_scheme =
  Base.Set.empty (module Base.Int), TArr (TList (TVar (-1)), TGround Int)
;;

let concat_lists_scheme =
  ( Base.Set.empty (module Base.Int)
  , TArr (TList (TVar (-1)), TArr (TList (TVar (-1)), TList (TVar (-1)))) )
;;

let concat_strings_scheme =
  ( Base.Set.empty (module Base.Int)
  , TArr (TGround String, TArr (TGround String, TGround String)) )
;;

let print_tuple_scheme = Base.Set.empty (module Base.Int), TArr (TVar (-1), TGround Int)

let print_string_scheme =
  Base.Set.empty (module Base.Int), TArr (TGround String, TGround Int)
;;

let compare_lists_scheme =
  ( Base.Set.empty (module Base.Int)
  , TArr (TList (TVar (-2)), TArr (TList (TVar (-2)), TGround Bool)) )
;;

let compare_tuples_scheme =
  Base.Set.empty (module Base.Int), TArr (TVar (-3), TArr (TVar (-3), TGround Bool))
;;

let compare_strings_scheme =
  ( Base.Set.empty (module Base.Int)
  , TArr (TGround String, TArr (TGround String, TGround Bool)) )
;;

let stdlib =
  [ "print_int", print_int_scheme
  ; "print_char", print_char_scheme
  ; "print_bool", print_bool_scheme
  ; "print_list", print_list_scheme
  ; "print_tuple", print_tuple_scheme
  ; "print_string", print_string_scheme
  ; "print_new_line", print_new_line_scheme
  ; "compare_lists_eq", compare_lists_scheme
  ; "compare_lists_neq", compare_lists_scheme
  ; "compare_lists_gt", compare_lists_scheme
  ; "compare_lists_gte", compare_lists_scheme
  ; "compare_lists_lt", compare_lists_scheme
  ; "compare_lists_lte", compare_lists_scheme
  ; "concat_lists", concat_lists_scheme
  ; "compare_tuples_eq", compare_tuples_scheme
  ; "compare_tuples_neq", compare_tuples_scheme
  ; "compare_tuples_gt", compare_tuples_scheme
  ; "compare_tuples_gte", compare_tuples_scheme
  ; "compare_tuples_lt", compare_tuples_scheme
  ; "compare_tuples_lte", compare_tuples_scheme
  ; "compare_strings_eq", compare_strings_scheme
  ; "compare_strings_neq", compare_strings_scheme
  ; "compare_strings_gt", compare_strings_scheme
  ; "compare_strings_gte", compare_strings_scheme
  ; "compare_strings_lt", compare_strings_scheme
  ; "compare_strings_lte", compare_strings_scheme
  ; "concat_strings", concat_strings_scheme
  ]
;;
