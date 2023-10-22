(** Copyright 2022-2023, Lev Golofastov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
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
