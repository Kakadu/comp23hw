(** Copyright 2023, Lev Golofastov & Ksenia Kuzmina *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Anf
open Format

let show_prim_ty =
  let open Typing in
  function
  | BoolTy -> "bool"
  | IntTy -> "int"
  | UnitTy -> "unit"
  | StringTy -> "string"
;;

let rec pp_ty ppf = function
  | Typing.VarTy n -> fprintf ppf "'_%d" n
  | PrimTy s -> fprintf ppf "%s" (show_prim_ty s)
  | ArrowTy (l, r) -> fprintf ppf "(%a -> %a)" pp_ty l pp_ty r
;;

let pp_scheme ppf = function
  | Typing.S (xs, t) -> fprintf ppf "forall %a . %a" Typing.VarSet.pp xs pp_ty t
;;

let pp_expr =
  (* we duplicate the code so as not to create a separate module for one function *)
  let is_op = function
    | "+" | "-" | "*" | "/" | "=" | "^" -> true
    | _ -> false
  in
  let pp_rec ppf = function
    | RecF -> fprintf ppf "rec "
    | NRecF -> fprintf ppf ""
  in
  let rec pp ppf = function
    | EConst const ->
      (match const with
       | CInt n -> fprintf ppf "%d" n
       | CBool true -> fprintf ppf "true"
       | CBool false -> fprintf ppf "false"
       | CString str -> fprintf ppf "%S" str
       | CUnit -> fprintf ppf "()")
    | EIfElse (c, th, el) -> fprintf ppf "if %a then %a else %a" pp c pp th pp el
    | EVar s -> pp_print_string ppf s
    | EApp (EVar str, r) when is_op str ->
      pp ppf (EApp (EVar (String.concat "" [ "("; str; ")" ]), r))
    | EApp (l, r) -> fprintf ppf "(%a %a)" pp l pp r
    | ELam (PatVar name, e) -> fprintf ppf "(fun %s -> %a)" name pp e
    | ELet ((f, PatVar name, body), in_e) ->
      fprintf ppf "let %a%s = %a in %a" pp_rec f name pp body pp in_e
  in
  pp
;;

let pp_anf_prog ppf p =
  let pp_imm ppf i =
    match i with
    | ImmBool true -> fprintf ppf "true"
    | ImmBool false -> fprintf ppf "false"
    | ImmNum i -> fprintf ppf "%d" i
    | ImmUnit -> fprintf ppf "()"
    | ImmString s -> fprintf ppf "\"%s\"" s
    | ImmValue v -> fprintf ppf "%s" v
  in
  let rec pp_aex ppf e =
    match e with
    | CApp (a, b) -> fprintf ppf "(%a %a)" pp_imm a pp_imm b
    | CIfElse (i, t, e) ->
      fprintf ppf "if %a then\n%a  else%a" pp_imm i pp_ablock t pp_ablock e
    | CImm i -> pp_imm ppf i
  and pp_alet ppf e =
    let name, expr = e in
    fprintf ppf "  %s = %a\n" name pp_aex expr
  and pp_ablock ppf b =
    let res, lets = b in
    List.iter (pp_alet ppf) lets;
    fprintf ppf "  %a\n" pp_imm res
  in
  let pp_afun ppf f =
    let name, args, body = f in
    let args = String.concat ", " args in
    fprintf ppf "fn %s %s =\n%a" name args pp_ablock body
  in
  let funs, prog = p in
  List.iter (pp_afun ppf) funs;
  fprintf ppf "fn main =\n";
  pp_ablock ppf prog
;;
