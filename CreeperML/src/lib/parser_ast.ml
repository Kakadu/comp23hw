(** Copyright 2023-2024, Arthur Alekseev and Starcev Matvey *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

module ParserAst = struct
  open Position.Position

  type name = string [@@deriving show { with_path = false }]
  type rec_flag = Rec | NoRec [@@deriving show { with_path = false }]

  type lvalue = LvAny | LvUnit | LvValue of name | LvTuple of loc_lvalue list
  and loc_lvalue = lvalue position [@@deriving show { with_path = false }]

  type literal =
    | LInt of int
    | LFloat of float
    | LString of string
    | LBool of bool
    | LUnit

  and loc_literal = literal position [@@deriving show { with_path = false }]

  type let_binding = { rec_f : rec_flag; l_v : loc_lvalue; body : loc_let_body }
  and let_body = { lets : loc_let_binding list; expr : loc_expr }

  and expr =
    | EApply of loc_expr * loc_expr
    | ELiteral of loc_literal
    | EValue of name
    | EFun of { lvalue : loc_lvalue; body : loc_let_body }
    | ETuple of loc_expr list
    | EIfElse of { cond : loc_expr; t_body : loc_expr; f_body : loc_expr }

  and loc_let_binding = let_binding position
  and loc_let_body = let_body position
  and loc_expr = expr position [@@deriving show { with_path = false }]

  type program = loc_let_binding list [@@deriving show { with_path = false }]
end

module ParserAstUtils = struct
  open ParserAst
  open Position.Position

  let rec_f = Rec
  let norec_f = NoRec
  let lv_any start_p end_p = LvAny |> with_position start_p end_p
  let lv_unit start_p end_p = LvUnit |> with_position start_p end_p
  let lv_value start_p end_p n = LvValue n |> with_position start_p end_p
  let lv_tuple start_p end_p lvs = LvTuple lvs |> with_position start_p end_p
  let l_int start_p end_p n = LInt n |> with_position start_p end_p
  let l_float start_p end_p n = LFloat n |> with_position start_p end_p
  let l_string start_p end_p s = LString s |> with_position start_p end_p
  let l_bool start_p end_p f = LBool f |> with_position start_p end_p
  let l_unit start_p end_p = LUnit |> with_position start_p end_p

  let e_apply start_p end_p e1 e2 =
    EApply (e1, e2) |> with_position start_p end_p

  let e_literal start_p end_p l = ELiteral l |> with_position start_p end_p
  let e_value start_p end_p n = EValue n |> with_position start_p end_p

  let e_fun start_p end_p l b =
    EFun { lvalue = l; body = b } |> with_position start_p end_p

  let e_tuple start_p end_p es = ETuple es |> with_position start_p end_p

  let e_if_else start_p end_p c t f =
    EIfElse { cond = c; t_body = t; f_body = f } |> with_position start_p end_p

  let let_binding ?(rec_flag = norec_f) start_p end_p lv body =
    { rec_f = rec_flag; l_v = lv; body } |> with_position start_p end_p

  let let_body start_p end_p ls e =
    { lets = ls; expr = e } |> with_position start_p end_p
end
