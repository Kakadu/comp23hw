(** Copyright 2023-2024, Arthur Alekseev and Starcev Matvey *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

module InferType = struct
  open Parser_ast.ParserAst

  type lvl = int [@@deriving show { with_path = false }]

  type ground_typ = TInt | TString | TBool | TUnit | TFloat
  [@@deriving show { with_path = false }]

  type 'a lvls = { value : 'a; mutable old_lvl : lvl; mutable new_lvl : lvl }
  [@@deriving show { with_path = false }]

  type ty =
    | TArrow of typ * typ
    | TTuple of typ list
    | TGround of ground_typ
    | TVar of tv ref

  and typ = ty lvls [@@deriving show { with_path = false }]
  and tv = Unbound of name * lvl | Link of typ

  type env = (name * typ) list [@@deriving show { with_path = false }]
end

module InferTypeUtils = struct
  open Parser_ast.ParserAst
  open InferType

  let t_int = TInt
  let t_string = TString
  let t_bool = TBool
  let t_unit = TUnit
  let t_float = TFloat
  let t_arrow t1 t2 = TArrow (t1, t2)
  let t_tuple ts = TTuple ts
  let t_ground t = TGround t
  let t_var t = TVar t
  let tv_unbound n l = Unbound (n, l)
  let tv_link t = Link t
  let lvl_value { value = v; old_lvl = _; new_lvl = _ } = v

  let is_unit typ =
    match lvl_value typ with TGround TUnit -> true | _ -> false

  let with_lvls old_l new_l v = { value = v; old_lvl = old_l; new_lvl = new_l }
  let assoc = List.assoc

  let convert_const l =
    let l = Position.Position.value l in
    (match l with
    | LInt _ -> TInt
    | LString _ -> TString
    | LBool _ -> TBool
    | LFloat _ -> TFloat
    | LUnit -> TUnit)
    |> t_ground

  let rec repr t =
    match lvl_value t with
    | TVar ({ contents = Link t } as tvar) ->
        let t = repr t in
        tvar := Link t;
        t
    | _ -> t
end

module TypeAst = struct
  open InferType
  open Parser_ast.ParserAst

  type ty =
    | TyArrow of ty * ty
    | TyTuple of ty list
    | TyGround of ground_typ
    | TyVar of name
  [@@deriving show { with_path = false }]

  type ('a, 'b) typed = { value : 'a; typ : 'b }
  [@@deriving show { with_path = false }]

  type 'ty typ_lvalue = (lvalue, 'ty) typed
  [@@deriving show { with_path = false }]

  type 'ty typ_let_binding = {
    rec_f : rec_flag;
    l_v : 'ty typ_lvalue;
    body : 'ty typ_let_body;
  }

  and 'ty typ_let_body = {
    lets : 'ty typ_let_binding list;
    expr : 'ty typ_expr;
  }

  and 'ty t_expr =
    | TApply of 'ty typ_expr * 'ty typ_expr
    | TLiteral of literal
    | TValue of name
    | TFun of 'ty tfun_body
    | TTuple of 'ty typ_expr list
    | TIfElse of 'ty tif_else

  and 'ty tfun_body = { lvalue : 'ty typ_lvalue; b : 'ty typ_let_body }

  and 'ty tif_else = {
    cond : 'ty typ_expr;
    t_body : 'ty typ_expr;
    f_body : 'ty typ_expr;
  }

  and 'ty typ_expr = ('ty t_expr, 'ty) typed
  [@@deriving show { with_path = false }]

  type 'ty typ_program = 'ty typ_let_binding list
  [@@deriving show { with_path = false }]

  let rec show_ty = function
    | TyArrow (x, y) -> Format.sprintf "%s -> %s" (show_ty x) (show_ty y)
    | TyGround TInt -> "int"
    | TyGround TString -> "string"
    | TyGround TBool -> "bool"
    | TyGround TUnit -> "()"
    | TyGround TFloat -> "float"
    | TyTuple xs ->
        List.map show_ty xs |> String.concat ", " |> Format.sprintf "(%s)"
    | TyVar n -> Format.sprintf "'%s" n
end

module TypeAstUtils = struct
  open TypeAst
  open InferTypeUtils

  let ty_arrow t1 t2 = TyArrow (t1, t2)
  let ty_tuple ts = TyTuple ts
  let ty_ground t = TyGround t
  let ty_var n = TyVar n
  let typed_value { value = v; typ = _ } = v
  let with_typ t v = { value = v; typ = t }
  let typ { value = _; typ = t } = t
  let ty_typ : ('a, ty) typed -> ty = fun { value = _; typ = t } -> t
  let typ_let_binding rec_f l_v b = { rec_f; l_v; body = b }
  let typ_let_body ls e = { lets = ls; expr = e }
  let t_apply e1 e2 = TApply (e1, e2)
  let t_literal l = TLiteral l
  let t_value n = TValue n
  let t_fun l_v b = TFun { lvalue = l_v; b }
  let t_tuple es = TTuple es
  let t_if_else c t f = TIfElse { cond = c; t_body = t; f_body = f }

  let rec remove_lvl typ =
    let open InferType in
    match lvl_value typ with
    | TVar { contents = Unbound (n, _) } -> ty_var n
    | TVar { contents = Link t } -> remove_lvl t
    | TArrow (t1, t2) -> ty_arrow (remove_lvl t1) (remove_lvl t2)
    | TTuple ts -> List.map remove_lvl ts |> ty_tuple
    | TGround t -> ty_ground t

  let rec convert_expr : InferType.typ typ_expr -> ty typ_expr =
   fun { value = expr; typ } ->
    let e : ty t_expr =
      match expr with
      | TApply (l, r) -> convert_expr r |> t_apply (convert_expr l)
      | TLiteral l -> t_literal l
      | TValue n -> t_value n
      | TTuple es -> List.map convert_expr es |> t_tuple
      | TIfElse { cond; t_body; f_body } ->
          t_if_else (convert_expr cond) (convert_expr t_body)
            (convert_expr f_body)
      | TFun { lvalue; b } ->
          with_typ (remove_lvl lvalue.typ) lvalue.value |> fun l_v ->
          convert_body b |> t_fun l_v
    in
    e |> with_typ (remove_lvl typ)

  and convert_body : InferType.typ typ_let_body -> ty typ_let_body =
   fun body ->
    typ_let_body (List.map convert_let body.lets) (convert_expr body.expr)

  and convert_let : InferType.typ typ_let_binding -> ty typ_let_binding =
   fun { rec_f; l_v; body } ->
    let l_v = with_typ (remove_lvl l_v.typ) l_v.value in
    convert_body body |> typ_let_binding rec_f l_v
end
