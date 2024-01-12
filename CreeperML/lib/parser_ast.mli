(** Copyright 2023-2024, Arthur Alekseev and Starcev Matvey *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

module ParserAst : sig
  open Position.Position

  type name = string
  type rec_flag = Rec  (** Recursive *) | NoRec  (** Not recursive *)

  type lvalue =
    | LvAny  (** Value _ *)
    | LvUnit  (** Value () *)
    | LvValue of name  (** Values like a *)
    | LvTuple of loc_lvalue list  (** Values like (a, 3) *)

  and loc_lvalue = lvalue position

  type literal =
    | LInt of int  (** Literals like 3 *)
    | LFloat of float  (** Literals like 3.2 *)
    | LString of string  (** Literals like "abc" *)
    | LBool of bool  (** Literals true/false *)
    | LUnit  (** Literal () *)

  type loc_literal = literal position

  type let_binding = {
    (* is recursive, name, names of args, body *)
    rec_f : rec_flag;
    l_v : loc_lvalue;
    args : loc_lvalue list;
    body : loc_let_body;
  }

  and loc_let_binding = let_binding position

  (*
      let x = .... (in?)
      let x = .... (in?)
      expr  
    *)
  and let_body = { lets : loc_let_binding list; expr : loc_expr }
  and loc_let_body = let_body position

  and expr =
    | EApply of loc_expr * loc_expr  (** e e *)
    | ELiteral of loc_literal  (** "among" or 90 *)
    | EValue of name  (** a *)
    | EFun of { lvalue : loc_lvalue; body : loc_let_body }
        (** fun x -> fun y -> ...
       fun x y -> ... represented as EFun x {[], EFun y ...} *)
    | ETuple of loc_expr list  (** (a, b, c) *)
    | EIfElse of { cond : loc_expr; t_body : loc_expr; f_body : loc_expr }
        (** if else statement *)

  and loc_expr = expr position

  type program = loc_let_binding list

  (* shows *)
  val show_name : name -> string
  val show_rec_flag : rec_flag -> string
  val show_lvalue : lvalue -> string
  val show_literal : literal -> string
  val show_let_binding : let_binding -> string
  val show_let_body : let_body -> string
  val show_expr : expr -> string
  val show_program : program -> string
  val show_loc_lvalue : loc_lvalue -> string
  val show_loc_literal : loc_literal -> string
  val show_loc_let_binding : loc_let_binding -> string
  val show_loc_let_body : loc_let_body -> string
  val show_loc_expr : loc_expr -> string

  (* pps *)
  val pp_name : Format.formatter -> name -> unit
  val pp_rec_flag : Format.formatter -> rec_flag -> unit
  val pp_lvalue : Format.formatter -> lvalue -> unit
  val pp_literal : Format.formatter -> literal -> unit
  val pp_let_binding : Format.formatter -> let_binding -> unit
  val pp_let_body : Format.formatter -> let_body -> unit
  val pp_expr : Format.formatter -> expr -> unit
  val pp_program : Format.formatter -> program -> unit
  val pp_loc_lvalue : Format.formatter -> loc_lvalue -> unit
  val pp_loc_literal : Format.formatter -> loc_literal -> unit
  val pp_loc_let_binding : Format.formatter -> loc_let_binding -> unit
  val pp_loc_let_body : Format.formatter -> loc_let_body -> unit
  val pp_loc_expr : Format.formatter -> loc_expr -> unit
end

module ParserAstUtils : sig
  open ParserAst
  open Position.Position

  (* recursive flags *)
  val rec_f : rec_flag
  val norec_f : rec_flag
  val is_rec : rec_flag -> bool

  (* left values *)
  val lv_any : t -> t -> loc_lvalue
  val lv_unit : t -> t -> loc_lvalue
  val lv_value : t -> t -> name -> loc_lvalue
  val lv_tuple : t -> t -> loc_lvalue list -> loc_lvalue

  (* literals *)
  val l_int : t -> t -> int -> loc_literal
  val l_float : t -> t -> float -> loc_literal
  val l_string : t -> t -> string -> loc_literal
  val l_bool : t -> t -> bool -> loc_literal
  val l_unit : t -> t -> loc_literal

  (* expressions *)
  val e_apply : t -> t -> loc_expr -> loc_expr -> loc_expr
  val e_literal : t -> t -> loc_literal -> loc_expr
  val e_value : t -> t -> name -> loc_expr
  val e_fun : t -> t -> loc_lvalue -> loc_let_body -> loc_expr
  val e_tuple : t -> t -> loc_expr list -> loc_expr
  val e_if_else : t -> t -> loc_expr -> loc_expr -> loc_expr -> loc_expr

  (* lets *)
  val let_binding :
    ?rec_flag:rec_flag ->
    t ->
    t ->
    loc_lvalue ->
    loc_lvalue list ->
    loc_let_body ->
    loc_let_binding

  val let_body : t -> t -> loc_let_binding list -> loc_expr -> loc_let_body
  val body : let_binding -> loc_let_body
  val args : let_binding -> loc_lvalue list
  val expr_b : let_body -> loc_expr
  val lets : let_body -> loc_let_binding list

  (* from lvalue, list of lvalues and body build chain of EFun-s*)
  val build_mul_e_fun :
    t -> t -> loc_lvalue -> loc_lvalue list -> loc_let_body -> expr position

  (* from list of lvalues and body build body of multyarg let *)
  val build_let_body : t -> t -> loc_lvalue list -> loc_let_body -> loc_let_body
end
