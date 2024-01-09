(** Copyright 2023-2024, Rustam Shangareev and Danil Yevdokimov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

val show_const : Ast.const -> string
val pp_const : Format.formatter -> Ast.const -> unit
val show_bin_op : Ast.bin_op -> string
val pp_bin_op : Format.formatter -> Ast.bin_op -> unit
val show_un_op : Ast.un_op -> string
val pp_un_op : Format.formatter -> Ast.un_op -> unit
val show_pt : Ast.pt -> string
val pp_pt : Format.formatter -> Ast.pt -> unit
val show_exp : Ast.exp -> string
val pp_exp : Format.formatter -> Ast.exp -> unit
val show_binding : bool * Ast.pt * Ast.exp -> string
val pp_binding : Format.formatter -> bool * Ast.pt * Ast.exp -> unit
val show_decl : Ast.decl -> string
val pp_decl : Format.formatter -> Ast.decl -> unit
val show_prog : Ast.decl list -> string
val pp_prog : Format.formatter -> Ast.decl list -> unit
