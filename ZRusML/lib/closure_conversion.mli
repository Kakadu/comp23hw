(** Copyright 2023-2024, Rustam Shangareev and Danil Yevdokimov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

(** Decompose fun to fun args list and fun body *)
val decompose_fun : Ast.pt list -> Ast.exp -> Ast.pt list * Ast.exp

(** Transform decls to decls without closures *)
val transform_decls : Ast.decl list -> Ast.decl list

