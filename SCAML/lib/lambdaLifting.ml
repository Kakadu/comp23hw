(** Copyright 2023-2024, Grigory Aseev and Matvey Kalashnikov *)

(** SPDX-License-Identifier: LGPL-3.0 *)

open Ast
open Llast
open Counter
open Base
open IState
open IState.Syntax

let rec get_args acc = function
  | EFun (p, e) -> get_args (p :: acc) e
  | e -> List.rev acc, e
;;

let rec get_global_scope prog global =
  match prog with
  | [] -> global
  | ELet (_, id, _) :: tl -> Set.add global id |> get_global_scope tl
;;

let rec gen_fresh_id global =
  let* fresh_id = fresh_name_int in
  let new_id = "id_" ^ Int.to_string fresh_id in
  let is_in = Set.mem global new_id in
  if is_in then gen_fresh_id global else return new_id
;;

let prog_lift prog =
  let rec lift_expr env ll_list global = function
    | EConst c ->
      (match c with
       | CInt i -> return (LLConst (CInt i), ll_list)
       | CBool b -> return (LLConst (CBool b), ll_list)
       | CUnit -> return (LLConst CUnit, ll_list))
    | EVar x ->
      (match Map.find env x with
       | Some id -> return (LLVar id, ll_list)
       | None -> return (LLVar x, ll_list))
    | EBinOp (op, e1, e2) ->
      let* l1, ll_list = lift_expr env ll_list global e1 in
      let* l2, ll_list = lift_expr env ll_list global e2 in
      return (LLBinOp (op, l1, l2), ll_list)
    | EIf (e1, e2, e3) ->
      let* l1, ll_list = lift_expr env ll_list global e1 in
      let* l2, ll_list = lift_expr env ll_list global e2 in
      let* l3, ll_list = lift_expr env ll_list global e3 in
      return (LLIf (l1, l2, l3), ll_list)
    | EApp (e1, e2) ->
      let* l1, ll_list = lift_expr env ll_list global e1 in
      let* l2, ll_list = lift_expr env ll_list global e2 in
      return (LLApp (l1, l2), ll_list)
    | ELetIn (is_rec, id, (EFun (_, _) as e1), e2) ->
      let args, exp = get_args [] e1 in
      let* new_id = gen_fresh_id global in
      let new_env = Map.set env ~key:id ~data:new_id in
      let* l1, ll_list =
        if is_rec
        then lift_expr new_env ll_list global exp
        else lift_expr env ll_list global exp
      in
      let ll_list = LLLet (is_rec, new_id, args, l1) :: ll_list in
      let* l2, ll_list = lift_expr new_env ll_list global e2 in
      return (l2, ll_list)
    | ELetIn (_, id, e1, e2) ->
      let* l1, ll_list = lift_expr env ll_list global e1 in
      let* l2, ll_list = lift_expr env ll_list global e2 in
      return (LLLetIn (id, l1, l2), ll_list)
    | EFun (_, _) as e ->
      let args, exp = get_args [] e in
      let* new_id = gen_fresh_id global in
      let* l1, ll_list = lift_expr (Map.empty (module String)) ll_list global exp in
      let ll_list = LLLet (false, new_id, args, l1) :: ll_list in
      return (LLVar new_id, ll_list)
  in
  let decl_ll global = function
    | ELet (is_rec, id, e) ->
      let p, expr = get_args [] e in
      let* l1, ll_list = lift_expr (Map.empty (module String)) [] global expr in
      let ll_list = LLLet (is_rec, id, p, l1) :: ll_list in
      return ll_list
  in
  let prog_ll prog =
    let global = get_global_scope prog (Set.empty (module String)) in
    let* ll_list =
      List.fold_left
        ~f:(fun acc x ->
          let* ll_list = decl_ll global x in
          let* acc = acc in
          return (acc @ List.rev ll_list))
        ~init:(return [])
        prog
    in
    return ll_list
  in
  prog_ll prog
;;

let run_ll prog = snd @@ IState.runState ~init:0 (prog_lift prog)
