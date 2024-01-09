(** Copyright 2023-2024, Rustam Shangareev and Danil Yevdokimov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

open Ast
module StringMap = Map.Make (String)

let rec replace_vars old_name new_name = function
  | EVar t when t = old_name -> EVar new_name
  | EUnOp (op, e) -> EUnOp (op, replace_vars old_name new_name e)
  | ELet (bindings, e) ->
    let new_bindings =
      List.map (fun (r, p, exp) -> r, p, replace_vars old_name new_name exp) bindings
    in
    ELet (new_bindings, replace_vars old_name new_name e)
  | EFun (p, e) -> EFun (p, replace_vars old_name new_name e)
  | EIf (e1, e2, e3) ->
    EIf
      ( replace_vars old_name new_name e1
      , replace_vars old_name new_name e2
      , replace_vars old_name new_name e3 )
  | EBinOp (op, e1, e2) ->
    EBinOp (op, replace_vars old_name new_name e1, replace_vars old_name new_name e2)
  | EApp (e1, e2) ->
    EApp (replace_vars old_name new_name e1, replace_vars old_name new_name e2)
  | _ as original -> original
;;

let rec lift_exp map cnt exp =
  match exp with
  | EUnOp (op, e) ->
    let new_map, new_cnt, new_e = lift_exp map cnt e in
    new_map, new_cnt, EUnOp (op, new_e)
  | ELet (bindings, e) ->
    let (map, cnt), new_bindings =
      List.fold_left_map
        (fun (acc_map, acc_cnt) (is_rec, pt, elem) ->
          let next_map, next_cnt, new_elem = lift_exp acc_map acc_cnt elem in
          let next_map =
            match elem with
            | EFun _ when is_rec ->
              let last_name = string_of_int (next_cnt - 1) ^ "lambda" in
              let (DLet (_, p, e)) = StringMap.find last_name next_map in
              let new_e =
                match pt with
                | PtVar old_name -> replace_vars old_name last_name e
                | _ -> e
              in
              StringMap.add
                last_name
                (DLet (true, p, new_e))
                (StringMap.remove last_name next_map)
            | _ -> next_map
          in
          (next_map, next_cnt), (is_rec, pt, new_elem))
        (map, cnt)
        bindings
    in
    let final_map, final_cnt, new_e = lift_exp map cnt e in
    final_map, final_cnt, ELet (new_bindings, new_e)
  | EFun _ ->
    let rec helper_efun = function
      | EFun (_, ex) -> helper_efun ex
      | other -> other
    in
    let e = helper_efun exp in
    let map, cnt, new_e = lift_exp map cnt e in
    let name = string_of_int cnt ^ "lambda" in
    let rec helper_result = function
      | EFun (p, e) -> EFun (p, helper_result e)
      | _ -> new_e
    in
    let new_efun = helper_result exp in
    let new_map = StringMap.add name (DLet (false, PtVar name, new_efun)) map in
    new_map, cnt + 1, EVar name
  | EIf (e1, e2, e3) ->
    let map1, cnt1, exp1 = lift_exp map cnt e1 in
    let map2, cnt2, exp2 = lift_exp map1 cnt1 e2 in
    let map3, cnt3, exp3 = lift_exp map2 cnt2 e3 in
    map3, cnt3, EIf (exp1, exp2, exp3)
  | EBinOp (op, e1, e2) ->
    let map1, cnt1, exp1 = lift_exp map cnt e1 in
    let map2, cnt2, exp2 = lift_exp map1 cnt1 e2 in
    map2, cnt2, EBinOp (op, exp1, exp2)
  | EApp (e1, e2) ->
    let map1, cnt1, exp1 = lift_exp map cnt e1 in
    let map2, cnt2, exp2 = lift_exp map1 cnt1 e2 in
    map2, cnt2, EApp (exp1, exp2)
  | _ -> map, cnt, exp
;;

let lift_decl map cnt (DLet (is_rec, pt, exp)) =
  let new_map, new_cnt, new_exp = lift_exp map cnt exp in
  new_map, new_cnt, DLet (is_rec, pt, new_exp)
;;

let lift_prog prog =
  let (new_bindings, _), new_prog =
    List.fold_left_map
      (fun (acc_map, acc_cnt) (DLet (is_rec, pt, elem)) ->
        let next_map, next_cnt, new_elem = lift_exp acc_map acc_cnt elem in
        let next_map =
          match elem with
          | EFun _ when is_rec ->
            let last_name = string_of_int (next_cnt - 1) ^ "lambda" in
            let (DLet (_, p, e)) = StringMap.find last_name next_map in
            let new_e =
              match pt with
              | PtVar old_name -> replace_vars old_name last_name e
              | _ -> e
            in
            StringMap.add
              last_name
              (DLet (true, p, new_e))
              (StringMap.remove last_name next_map)
          | _ -> next_map
        in
        (next_map, next_cnt), DLet (is_rec, pt, new_elem))
      (StringMap.empty, 0)
      prog
  in
  let new_bindings = List.map snd @@ List.of_seq @@ StringMap.to_seq new_bindings in
  new_bindings @ new_prog
;;
