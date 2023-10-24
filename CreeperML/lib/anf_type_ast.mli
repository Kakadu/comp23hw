module AnfTypeAst : sig
  open Type_ast.TypeAst
  open Parser_ast.ParserAst
  open Position.Position
  open Closureconvert.ClosureAst
  open Db.DbTypeAst

  type tlvalue = db_lvalue
  type tliteral = (literal, ty) typed
  type tname = (int, ty) typed
  type imm = ImmVal of tname | ImmLit of tliteral

  type anf_expr =
    | AApply of imm * imm
    | ATuple of imm list
    | Aite of anf_body * anf_body * anf_body
    | AImm of imm
    | ATupleAccess of imm * int

  and anf_body = { lets : anf_val_binding list; res : imm }
  and anf_val_binding = { name : tname; e : anf_expr }

  type anf_fun_binding = {
    name : tname;
    arg : tname;
    body : anf_body;
    env_vars : tname list;
  }

  type anf_binding = AnfVal of anf_val_binding | AnfFun of anf_fun_binding
  type anf_program = anf_binding list

  val show_anf_program : bool -> anf_binding list -> name
end

module AnfConvert : sig
  open AnfTypeAst
  open Closureconvert.ClosureAst

  val anf_of_program : cf_typ_program -> anf_program
end
