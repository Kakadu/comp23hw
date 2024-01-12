(** Copyright 2023-2024, Arthur Alekseev and Starcev Matvey *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

module AnfTypeAst : sig
  open Type_ast.TypeAst
  open Parser_ast.ParserAst
  open Indexed_ast.IndexedTypeAst

  type tlvalue = index_lvalue
  type tliteral = (literal, ty) typed
  type tname = (string, ty) typed

  type imm =
    | ImmVal of tname
    | ImmLit of tliteral
        (** Imm is either some result (a, b, c, etc) or literal (1, "2", etc)*)

  type anf_expr =
    | AApply of imm * imm list
        (** Application of immidiate function to N immidiate args *)
    | ATuple of imm list  (** Creation of tuple with N imm elements*)
    | Aite of imm * anf_body * anf_body
        (** If then else, where if part is pre-calculated, and then and else are contained like blocks for lazy evaluation*)
    | AImm of imm  (** Immidiate value (var or literal)*)
    | ATupleAccess of imm * int
        (** Accessing tuple, tuple is imm, index is int *)
    | AClosure of tname * imm list
        (** Allocation of closure with function tname and enviroment env *)

  and anf_body = { lets : anf_val_binding list; res : imm }
  (** ANF body: some lets and result of block, used in ite branches and funs *)

  and anf_val_binding = { name : tname; e : anf_expr }
  (** ANF Value binding, usual binding like let x = f 3 a *)

  type anf_fun_binding = {
    name : tname;
    args : tname list;
    env : tname list;
    body : anf_body;
  }
  (** ANF Function binding, let name [args] = [lets] res, all in ANF*)

  (** ANF Binding is either ANF Value or ANF Function*)
  type anf_binding =
    | AnfVal of anf_val_binding
    | AnfFun of anf_fun_binding  (** obv. *)

  type anf_program = anf_binding list
end

module AnfConvert : sig
  open AnfTypeAst
  open Closure.ClosureAst

  val anf_of_cf : cf_typ_program -> anf_program
end

module AnfOptimizations : sig
  open AnfTypeAst

  val optimize_moves : anf_program -> anf_program
end
