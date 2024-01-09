(** Copyright 2023-2024, Rustam Shangareev and Danil Yevdokimov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

open Ast
module StringMap = Map.Make (String)
module StringSet = Set.Make (String)

let rec decompose_fun pts = function
  | EFun (p, e) -> decompose_fun (p :: pts) e
  | e -> pts, e
;;

let rec transform_exp exp env exp_name =
  match exp with
  | EVar nm ->
    (match StringSet.find_opt nm env with
     | Some _ -> StringSet.empty, exp
     | None -> StringSet.singleton nm, exp)
  | EUnOp (op, e) ->
    let new_env, new_exp = transform_exp e env exp_name in
    new_env, EUnOp (op, new_exp)
  | EIf (e1, e2, e3) ->
    let env1, exp1 = transform_exp e1 env exp_name in
    let env2, exp2 = transform_exp e2 env exp_name in
    let env3, exp3 = transform_exp e3 env exp_name in
    StringSet.union (StringSet.union env1 env2) env3, EIf (exp1, exp2, exp3)
  | EBinOp (op, e1, e2) ->
    let env1, exp1 = transform_exp e1 env exp_name in
    let env2, exp2 = transform_exp e2 env exp_name in
    StringSet.union env1 env2, EBinOp (op, exp1, exp2)
  | EApp (e1, e2) ->
    let env1, exp1 = transform_exp e1 env exp_name in
    let env2, exp2 = transform_exp e2 env exp_name in
    StringSet.union env1 env2, EApp (exp1, exp2)
  | EFun (_, _) ->
    let vars, body = decompose_fun [] exp in
    let args =
      StringSet.of_list
        (List.filter_map
           (function
             | PtVar name -> Some name
             | _ -> None)
           vars)
    in
    let body_vars, new_body = transform_exp body args exp_name in
    let body_vars = StringSet.remove exp_name body_vars in
    let body_vars_lst = List.of_seq (StringSet.to_seq body_vars) in
    let pt_vars = List.map (fun elem -> PtVar elem) body_vars_lst @ List.rev vars in
    let rec helper_efun = function
      | hd :: tl -> EFun (hd, helper_efun tl)
      | _ -> new_body
    in
    let new_efun = helper_efun pt_vars in
    let rec helper_eapp = function
      | hd :: tl -> EApp (helper_eapp tl, hd)
      | _ -> new_efun
    in
    let new_eapp =
      helper_eapp (List.rev (List.map (fun elem -> EVar elem) body_vars_lst))
    in
    StringSet.diff body_vars env, new_eapp
  | ELet (bindings, e) ->
    let (bindings_acc, final_env), new_bindings =
      List.fold_left_map
        (fun (acc, prev_env) (is_rec, p, ex) ->
          let p_name =
            match p with
            | PtVar name -> name
            | _ -> "_"
          in
          let new_acc = if is_rec then StringSet.add p_name acc else acc in
          let next_env, new_ex = transform_exp ex new_acc p_name in
          let new_acc = StringSet.add p_name acc in
          (new_acc, StringSet.union next_env prev_env), (is_rec, p, new_ex))
        (env, StringSet.empty)
        bindings
    in
    let final_acc, new_e = transform_exp e bindings_acc exp_name in
    StringSet.union final_acc final_env, ELet (new_bindings, new_e)
  | _ -> StringSet.empty, exp
;;

let transform_decls bindings =
  let _, new_bindings =
    List.fold_left_map
      (fun acc (DLet (is_rec, p, ex)) ->
        let p_name =
          match p with
          | PtVar name -> name
          | _ -> "_"
        in
        let new_acc = if is_rec then StringSet.add p_name acc else acc in
        let _, new_ex = transform_exp ex new_acc p_name in
        let new_acc = StringSet.add p_name acc in
        new_acc, (is_rec, p, new_ex))
      StringSet.empty
      bindings
  in
  List.map (fun elem -> DLet elem) new_bindings
;;
