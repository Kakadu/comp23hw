(** Copyright 2023-2024, Arthur Alekseev and Starcev Matvey *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

module ClosureAst : sig
  open Indexed_ast.IndexedTypeAst
  open Parser_ast.ParserAst
  open Type_ast.TypeAst

  type cf_expr =
    | CFApply of cf_typ_expr * cf_typ_expr list
    | CFLiteral of literal
    | CFValue of string
    | CFTuple of cf_typ_expr list
    | CFIfElse of cf_if_else
    | CFClosure of string * (string, ty) typed list

  and cf_if_else = {
    cond : cf_typ_expr;
    t_body : cf_typ_expr;
    f_body : cf_typ_expr;
  }

  and cf_typ_expr = (cf_expr, ty) typed

  type cf_typ_let_binding = {
    rec_f : rec_flag;
    l_v : index_lvalue;
    cf_body : cf_typ_let_body;
  }

  and cf_typ_let_body = {
    cf_lets : cf_typ_let_binding list;
    cf_expr : cf_typ_expr;
  }

  type cf_fun_let_binding = {
    is_rec : rec_flag;
    name : (string, ty) typed;
    args : index_lvalue list;
    b : cf_typ_let_body;
    env_vars : (string, ty) typed list;
  }

  type cf_binding =
    | FunBinding of cf_fun_let_binding
    | ValBinding of cf_typ_let_binding

  type cf_typ_program = cf_binding list
end

module ClosureConvert : sig
  open ClosureAst
  open Indexed_ast.IndexedTypeAst

  val cf_of_index : index_program -> cf_typ_program
end
