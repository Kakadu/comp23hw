(** Copyright 2023-2024, Arthur Alekseev and Starcev Matvey *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

module IndexedTypeAst : sig
  open Type_ast.TypeAst
  open Parser_ast.ParserAst

  type ilvalue =
    | DLvAny  (** lvalue _ *)
    | DLvUnit  (** lvalue () *)
    | DLvValue of string  (** lvalue like a *)
    | DLvTuple of ilvalue list  (** lvalue like (lvalue, lvalue, ...) *)

  type index_lvalue = (ilvalue, ty) typed

  type index_let_binding = {
    rec_f : rec_flag;
    l_v : index_lvalue;
    body : index_let_body;
  }

  and index_let_body = { lets : index_let_binding list; expr : index_expr }

  and d_expr =
    | DApply of index_expr * index_expr  (** index expr like (expr) (expr) *)
    | DLiteral of literal  (** index expr like 123/"abc"/true/false/... *)
    | DValue of string  (** index expr like a/b/c *)
    | DFun of index_fun_body  (** index expr like fun arg -> ... *)
    | DTuple of index_expr list  (** index expr like (a, b, c) *)
    | DIfElse of tif_else  (** index expr like if a then b else c *)

  and index_fun_body = { lvalue : index_lvalue; b : index_let_body }
  and tif_else = { cond : index_expr; t_body : index_expr; f_body : index_expr }
  and index_expr = (d_expr, ty) typed

  type index_program = index_let_binding list

  val index_of_typed : ty typ_program -> index_program
end
