(** Copyright 2023-2024, Grigory Aseev and Matvey Kalashnikov *)

(** SPDX-License-Identifier: LGPL-3.0 *)

open Ast
open Base

let free_vars expr =
  let rec efun_helper set = function
    | EFun (PVar x, e) -> efun_helper (Set.add set x) e
    | EFun (_, e) -> efun_helper set e
    | _ -> set
  in
  let rec helper = function
    | EConst _ -> Set.empty (module String)
    | EVar x -> Set.singleton (module String) x
    | EBinOp (_, e1, e2) -> Set.union (helper e1) (helper e2)
    | EIf (e1, e2, e3) -> Set.union (Set.union (helper e1) (helper e2)) (helper e3)
    | EApp (e1, e2) -> Set.union (helper e1) (helper e2)
    | EFun (x, e) ->
      (match x with
       | PVar x -> Set.remove (helper e) x
       | _ -> helper e)
    | ELetIn (b, x, e1, e2) ->
      let free1 = helper e1 in
      let free1' = if b then Set.remove free1 x else free1 in
      let e1_pat = efun_helper (Set.empty (module String)) e1 in
      let free2 = Set.diff (helper e2) e1_pat in
      let free2' = Set.remove free2 x in
      Set.union free1' free2'
  in
  helper expr
;;

let closure_conversion global_env decl =
  let rec expr_closure local_env global_env = function
    | EConst x -> constr_econst x
    | EVar x as orig ->
      (match Map.find local_env x with
       | Some free ->
         let ids = List.map (Set.to_list free) ~f:(fun x -> constr_evar x) in
         constr_eapp orig ids
       | None -> orig)
    | EBinOp (op, e1, e2) ->
      let e1' = expr_closure local_env global_env e1 in
      let e2' = expr_closure local_env global_env e2 in
      constr_ebinop op e1' e2'
    | EIf (e1, e2, e3) ->
      let e1' = expr_closure local_env global_env e1 in
      let e2' = expr_closure local_env global_env e2 in
      let e3' = expr_closure local_env global_env e3 in
      constr_eif e1' e2' e3'
    | EApp (e1, e2) ->
      let e1' = expr_closure local_env global_env e1 in
      let e2' = expr_closure local_env global_env e2 in
      constr_eapp e1' [ e2' ]
    | EFun (x, _) as orig ->
      let s = free_vars orig in
      let s' = Set.diff s global_env in
      let e' = efun_helper local_env global_env orig in
      (match x with
       | PVar _ ->
         let fun_fold =
           constr_efun (List.map (Set.to_list s') ~f:(fun x -> constr_pvar x)) e'
         in
         constr_eapp fun_fold (List.map (Set.to_list s') ~f:(fun x -> constr_evar x))
       | _ -> e')
    | ELetIn (b, x, (EFun (_, _) as e1), e2) as orig ->
      let free = free_vars orig in
      let free' = Set.diff free global_env in
      let e1' = efun_helper local_env global_env e1 in
      let e1_closure =
        constr_efun (List.map (Set.to_list free') ~f:(fun x -> constr_pvar x)) e1'
      in
      let local_env' = Map.set local_env ~key:x ~data:free' in
      let e2' = expr_closure local_env' (Set.add global_env x) e2 in
      constr_eletin b x e1_closure e2'
    | ELetIn (b, x, e1, e2) ->
      let e1' = expr_closure local_env global_env e1 in
      let e2' = expr_closure local_env global_env e2 in
      constr_eletin b x e1' e2'
  and efun_helper local_env global_env = function
    | EFun ((PVar _ as orig), e) ->
      let e' = efun_helper local_env global_env e in
      constr_efun [ orig ] e'
    | EFun (p, e) ->
      let e' = efun_helper local_env global_env e in
      constr_efun [ p ] e'
    | expr -> expr_closure local_env global_env expr
  in
  let decl_closure global_env = function
    | ELet (is_rec, id, e) ->
      let global_env' = if is_rec then Set.add global_env id else global_env in
      let e' = efun_helper (Map.empty (module String)) global_env' e in
      constr_elet is_rec id e'
  in
  decl_closure global_env decl
;;

let prog_conversion program =
  let closured prog =
    List.fold
      ~init:([], Set.empty (module String))
      ~f:(fun (acc, global) ->
        function
        | ELet (_, id, _) as orig ->
          let e' = closure_conversion global orig in
          let global' = Set.add global id in
          e' :: acc, global')
      prog
  in
  List.rev (fst @@ closured program)
;;

let print_expr_result decl =
  let buf = closure_conversion (Set.empty (module String)) decl in
  Stdlib.Format.printf "%s" (Ast.show_binding buf)
;;

let%expect_test _ =
  print_expr_result
    (ELet
       ( false
       , "fac"
       , EFun
           ( PVar "n"
           , ELetIn
               ( true
               , "fack"
               , EFun
                   ( PVar "n"
                   , EFun
                       ( PVar "k"
                       , EIf
                           ( EBinOp (Leq, EVar "n", EConst (CInt 1))
                           , EApp (EVar "k", EConst (CInt 1))
                           , EApp
                               ( EApp
                                   (EVar "fack", EBinOp (Sub, EVar "n", EConst (CInt 1)))
                               , EFun
                                   ( PVar "m"
                                   , EApp (EVar "k", EBinOp (Mul, EVar "m", EVar "n")) )
                               ) ) ) )
               , EApp (EApp (EVar "fack", EVar "n"), EFun (PVar "x", EVar "x")) ) ) ));
  [%expect
    {|
      (ELet (false, "fac",
         (EFun ((PVar "n"),
            (ELetIn (true, "fack",
               (EFun ((PVar "n"),
                  (EFun ((PVar "k"),
                     (EIf ((EBinOp (Leq, (EVar "n"), (EConst (CInt 1)))),
                        (EApp ((EVar "k"), (EConst (CInt 1)))),
                        (EApp (
                           (EApp ((EVar "fack"),
                              (EBinOp (Sub, (EVar "n"), (EConst (CInt 1)))))),
                           (EApp (
                              (EApp (
                                 (EFun ((PVar "k"),
                                    (EFun ((PVar "n"),
                                       (EFun ((PVar "m"),
                                          (EApp ((EVar "k"),
                                             (EBinOp (Mul, (EVar "m"), (EVar "n")))
                                             ))
                                          ))
                                       ))
                                    )),
                                 (EVar "k"))),
                              (EVar "n")))
                           ))
                        ))
                     ))
                  )),
               (EApp ((EApp ((EVar "fack"), (EVar "n"))),
                  (EFun ((PVar "x"), (EVar "x")))))
               ))
            ))
         ))
         |}]
;;
