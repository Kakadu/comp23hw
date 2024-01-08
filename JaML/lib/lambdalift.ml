(** Copyright 2023-2024, Ilya Pankratov, Maxim Drumov *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open Base
open Typedtree
open Toplevel

(* Environment Map
   key -- string name of function;
   value -- tbinding:
   Stores, by function name, the function declaration that is to be placed outside the function afterwards.
*)
module EnvM = Base.Map.Poly

let extend_env env key data = EnvM.set env ~key ~data

let rec get_args_let known = function
  | TFun (Arg (id, ty), expr, _) -> get_args_let (Arg (id, ty) :: known) expr
  | _ -> known
;;

let rec lambda_lift_expr env = function
  | TConst (c, ty) -> LConst (c, ty), env
  | TVar (x, ty) -> LVar (x, ty), env
  | TFun (_, expr, _) -> lambda_lift_expr env expr
  | TBinop (op, e1, e2, ty) ->
    let e1, env = lambda_lift_expr env e1 in
    let e2, env = lambda_lift_expr env e2 in
    LBinop (op, e1, e2, ty), env
  | TApp (fst, scd, ty) ->
    let fst, env = lambda_lift_expr env fst in
    let scd, env = lambda_lift_expr env scd in
    LApp (fst, scd, ty), env
  | TIfThenElse (cond, e1, e2, ty) ->
    let cond, env = lambda_lift_expr env cond in
    let e1, env = lambda_lift_expr env e1 in
    let e2, env = lambda_lift_expr env e2 in
    LIfThenElse (cond, e1, e2, ty), env
  | TLetRecIn (id, e1, e2, ty) ->
    let args = List.rev @@ get_args_let [] e1 in
    let e1, env = lambda_lift_expr env e1 in
    let e2, env = lambda_lift_expr env e2 in
    let expr, env =
      if List.is_empty args
      then LLetRecIn (id, e1, e2, ty), env
      else e2, extend_env env id (LLetRec (id, args, e1, ty))
    in
    expr, env
  | TLetIn (id, e1, e2, ty) ->
    let args = List.rev @@ get_args_let [] e1 in
    let e1, env = lambda_lift_expr env e1 in
    let e2, env = lambda_lift_expr env e2 in
    let expr, env =
      if List.is_empty args
      then LLetIn (id, e1, e2, ty), env
      else e2, extend_env env id (LLet (id, args, e1, ty))
    in
    expr, env
;;

let lambda_lift_bindings env = function
  | TLet (id, expr, ty) ->
    let args = List.rev @@ get_args_let [] expr in
    let expr, env = lambda_lift_expr env expr in
    LLet (id, args, expr, ty), env
  | TLetRec (id, expr, ty) ->
    let args = List.rev @@ get_args_let [] expr in
    let expr, env = lambda_lift_expr env expr in
    LLetRec (id, args, expr, ty), env
;;

let lambda_lift expr =
  let empty = EnvM.empty in
  let stms =
    List.fold expr ~init:[] ~f:(fun stms el ->
      let stmt, env = lambda_lift_bindings empty el in
      let mapped_env = List.map ~f:(fun (_, expr) -> expr) (EnvM.to_alist env) in
      (stmt :: List.rev mapped_env) @ stms)
  in
  List.rev stms
;;
