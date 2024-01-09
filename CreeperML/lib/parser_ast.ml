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
  [@@deriving show { with_path = false }]

  type loc_literal = literal position [@@deriving show { with_path = false }]

  type let_binding = {
    rec_f : rec_flag;
    l_v : loc_lvalue;
    args : loc_lvalue list;
    body : loc_let_body;
  }

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
  let is_rec = function Rec -> true | NoRec -> false
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

  let let_binding ?(rec_flag = norec_f) start_p end_p lv args body =
    { rec_f = rec_flag; l_v = lv; args; body } |> with_position start_p end_p

  let let_body start_p end_p ls e =
    { lets = ls; expr = e } |> with_position start_p end_p

  let body { rec_f = _; l_v = _; args = _; body = b } = b
  let args { rec_f = _; l_v = _; args; body = _ } = args
  let expr_b { lets = _; expr = e } = e
  let lets { lets; expr = _ } = lets

  let build_mul_e_fun start_p end_p hd tl b =
    let b =
      position b |> fun { start_p; end_p } ->
      value b |> expr_b |> let_body start_p end_p (value b |> lets)
    in
    match List.rev tl with
    | [] -> e_fun start_p end_p hd b
    | l :: tl ->
        let e_body_fun arg body =
          let_body start_p end_p [] body |> e_fun start_p end_p arg
        in
        List.fold_left
          (fun acc l -> e_body_fun l acc)
          (e_fun start_p end_p l b) tl
        |> e_body_fun hd

  let build_let_body start_p end_p lvs b =
    match lvs with
    | [] -> b
    | hd :: tl ->
        build_mul_e_fun start_p end_p hd tl b |> let_body start_p end_p []
end

let%expect_test _ =
  Printf.printf {|Hello, world!|};
  [%expect {|Hello, world!|}]
