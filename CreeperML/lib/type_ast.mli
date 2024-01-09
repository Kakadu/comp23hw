(** Copyright 2023-2024, Arthur Alekseev and Starcev Matvey *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

module InferType : sig
  open Parser_ast.ParserAst

  (* inner types for inferencer that unused in other modules *)

  (* level of type nesting *)
  type lvl = int

  (* minimal type of expression *)
  type ground_typ =
    | TInt  (** int type *)
    | TString  (** string type *)
    | TBool  (** bool type *)
    | TUnit  (** unit type *)
    | TFloat  (** float type *)

  (* levels of type nesting (for infer) *)
  type 'a lvls = { value : 'a; mutable old_lvl : lvl; mutable new_lvl : lvl }

  (* complex type *)
  type ty =
    | TArrow of typ * typ  (** typ -> typ type *)
    | TTuple of typ list  (** (typ, ...) type *)
    | TGround of ground_typ  (** int/string/bool/unit/float type *)
    | TVar of tv ref  (** 'a type *)

  (* main type for infering *)
  and typ = ty lvls

  (* subtype for infer *)
  and tv =
    | Unbound of name * lvl  (** unbound type var *)
    | Link of typ  (** link to typ *)

  (* environment *)
  type env = (name * typ) list

  (* shows *)
  val show_lvl : lvl -> string
  val show_ground_typ : ground_typ -> string
  val show_ty : ty -> string
  val show_tv : tv -> string
  val show_lvls : (Format.formatter -> 'a -> unit) -> 'a lvls -> string
  val show_typ : typ -> string
  val show_env : env -> string

  (* pps *)
  val pp_lvl : Format.formatter -> lvl -> unit
  val pp_ground_typ : Format.formatter -> ground_typ -> unit
  val pp_ty : Format.formatter -> ty -> unit
  val pp_tv : Format.formatter -> tv -> unit

  val pp_lvls :
    (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a lvls -> unit

  val pp_typ : Format.formatter -> typ -> unit
  val pp_env : Format.formatter -> env -> unit
end

module InferTypeUtils : sig
  open Parser_ast.ParserAst
  open InferType

  val t_int : ground_typ
  val t_string : ground_typ
  val t_bool : ground_typ
  val t_unit : ground_typ
  val t_float : ground_typ
  val t_arrow : typ -> typ -> ty
  val t_tuple : typ list -> ty
  val t_ground : ground_typ -> ty
  val t_var : tv ref -> ty
  val tv_unbound : name -> lvl -> tv
  val tv_link : typ -> tv
  val is_unit : typ -> bool
  val lvl_value : 'a lvls -> 'a
  val with_lvls : lvl -> lvl -> 'a -> 'a lvls

  (* find in env *)
  val assoc : name -> env -> typ

  (* get type of const *)
  val convert_const : literal Position.Position.position -> ty

  (* simplifies links *)
  val repr : typ -> typ
end

module TypeAst : sig
  open InferType
  open Parser_ast.ParserAst

  (* outter typed ast that used by next modules *)

  (* types without lvlvs *)
  type ty =
    | TyArrow of ty * ty  (** ty -> ty type *)
    | TyTuple of ty list  (** (ty, ty, ..) type *)
    | TyGround of ground_typ  (** ground type *)
    | TyVar of name  (** 'a type *)

  (* expresion with his type *)
  (* InferType.typ to infer work *)
  (* TypeAst.ty to infer result *)
  type ('a, 'b) typed = { value : 'a; typ : 'b }
  type 'ty typ_lvalue = (lvalue, 'ty) typed

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
    | TApply of 'ty typ_expr * 'ty typ_expr  (** typed application (a b) *)
    | TLiteral of literal  (** typed literal (3; "123") *)
    | TValue of name  (** typed value (a; something) *)
    | TFun of 'ty tfun_body  (** typed fun (fun arg -> ...) *)
    | TTuple of 'ty typ_expr list  (** typed tuple ((3, b c, (1, 2)))*)
    | TIfElse of 'ty tif_else  (** typed if-then-else (if a then b else c)*)

  and 'ty tfun_body = { lvalue : 'ty typ_lvalue; b : 'ty typ_let_body }

  and 'ty tif_else = {
    cond : 'ty typ_expr;
    t_body : 'ty typ_expr;
    f_body : 'ty typ_expr;
  }

  and 'ty typ_expr = ('ty t_expr, 'ty) typed

  type 'ty typ_program = 'ty typ_let_binding list

  (* shows *)
  val show_ty : ty -> string

  val show_typed :
    (Format.formatter -> 'a -> unit) ->
    (Format.formatter -> 'b -> unit) ->
    ('a, 'b) typed ->
    string

  val show_typ_lvalue :
    (Format.formatter -> 'a -> unit) -> 'a typ_lvalue -> string

  val show_typ_let_binding :
    (Format.formatter -> 'a -> unit) -> 'a typ_let_binding -> string

  val show_typ_let_body :
    (Format.formatter -> 'a -> unit) -> 'a typ_let_body -> string

  val show_tfun_body :
    (Format.formatter -> 'a -> unit) -> 'a tfun_body -> string

  val show_tif_else : (Format.formatter -> 'a -> unit) -> 'a tif_else -> string
  val show_t_expr : (Format.formatter -> 'a -> unit) -> 'a t_expr -> string
  val show_typ_expr : (Format.formatter -> 'a -> unit) -> 'a typ_expr -> string

  val show_typ_program :
    (Format.formatter -> 'a -> unit) -> 'a typ_program -> string

  (* pps *)
  val pp_ty : Format.formatter -> ty -> unit

  val pp_typed :
    (Format.formatter -> 'a -> unit) ->
    (Format.formatter -> 'b -> unit) ->
    Format.formatter ->
    ('a, 'b) typed ->
    unit

  val pp_typ_lvalue :
    (Format.formatter -> 'a -> unit) ->
    Format.formatter ->
    'a typ_lvalue ->
    unit

  val pp_typ_let_binding :
    (Format.formatter -> 'a -> unit) ->
    Format.formatter ->
    'a typ_let_binding ->
    unit

  val pp_typ_let_body :
    (Format.formatter -> 'a -> unit) ->
    Format.formatter ->
    'a typ_let_body ->
    unit

  val pp_tfun_body :
    (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a tfun_body -> unit

  val pp_tif_else :
    (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a tif_else -> unit

  val pp_t_expr :
    (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t_expr -> unit

  val pp_typ_expr :
    (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a typ_expr -> unit

  val pp_typ_program :
    (Format.formatter -> 'a -> unit) ->
    Format.formatter ->
    'a typ_program ->
    unit
end

module TypeAstUtils : sig
  open Parser_ast.ParserAst
  open TypeAst

  val ty_arrow : ty -> ty -> ty
  val ty_tuple : ty list -> ty
  val ty_ground : InferType.ground_typ -> ty
  val ty_var : name -> ty
  val typed_value : ('a, 'b) typed -> 'a
  val with_typ : 'b -> 'a -> ('a, 'b) typed
  val typ : ('a, 'b) typed -> 'b
  val ty_typ : ('a, ty) typed -> ty

  val typ_let_binding :
    rec_flag -> 'a typ_lvalue -> 'a typ_let_body -> 'a typ_let_binding

  val typ_let_body : 'a typ_let_binding list -> 'a typ_expr -> 'a typ_let_body
  val t_apply : 'a typ_expr -> 'a typ_expr -> 'a t_expr
  val t_literal : literal -> 'a t_expr
  val t_value : name -> 'a t_expr
  val t_fun : 'a typ_lvalue -> 'a typ_let_body -> 'a t_expr
  val t_tuple : 'a typ_expr list -> 'a t_expr
  val t_if_else : 'a typ_expr -> 'a typ_expr -> 'a typ_expr -> 'a t_expr

  (* removes infer's levels from types *)
  val remove_lvl : InferType.typ -> ty

  (* convert inner ast type into outter ast type *)
  val convert_expr : InferType.typ typ_expr -> ty typ_expr
  val convert_let : InferType.typ typ_let_binding -> ty typ_let_binding
end
