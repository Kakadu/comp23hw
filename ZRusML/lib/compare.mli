(** Copyright 2023-2024, Rustam Shangareev and Danil Yevdokimov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

open Ast

val print_bin_op : bin_op -> string
val print_un_op : un_op -> string
val print_pt : pt -> string
val print_const : const -> string
val print_exp : exp -> string
val print_let : bool * pt * exp -> string
val print_bindings : (bool * pt * exp) list -> string
val print_prog : decl list -> string
