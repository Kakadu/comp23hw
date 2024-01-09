(** Copyright 2023-2024, Anton Kraev and Polina Badreeva *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ll_ast
open Cc_ast
open Counter
open Base

let gen_var = gen_var "Ll"
let extend_env env func = func :: env

let rec lift_expr env = function
  | CLiteral l -> LLiteral l, env
  | CIdentifier id -> LIdentifier id, env
  | CUnaryOp (op, body) ->
    let ll_body, env' = lift_expr env body in
    LUnaryOp (op, ll_body), env'
  | CBinaryOp (op, left, right) ->
    let ll_left, env' = lift_expr env left in
    let ll_right, env'' = lift_expr env' right in
    LBinaryOp (op, ll_left, ll_right), env''
  | CApp (left, right) ->
    let ll_left, env' = lift_expr env left in
    let ll_right, env'' = lift_expr env' right in
    LApp (ll_left, ll_right), env''
  | CAbs (args, body) ->
    let ll_body, env' = lift_expr env body in
    let new_fun_name = gen_var () in
    let ll_body_name = LIdentifier new_fun_name in
    ll_body_name, extend_env env' @@ LFun (new_fun_name, args, ll_body)
  | CIfThenElse (cond, if_true, if_false) ->
    let ll_cond, env' = lift_expr env cond in
    let ll_if_true, env'' = lift_expr env' if_true in
    let ll_if_false, env''' = lift_expr env'' if_false in
    LIfThenElse (ll_cond, ll_if_true, ll_if_false), env'''
  | CLetIn (binding, body) ->
    let ll_let_body, env' = lift_expr env body in
    (match binding with
     | CVal (id, body) ->
       let ll_val_body, env'' = lift_expr env' body in
       LLetIn (id, ll_val_body, ll_let_body), env''
     | CFun (id, args, body) ->
       let ll_fun_body, env'' = lift_expr env' body in
       ll_let_body, extend_env env'' @@ LFun (id, args, ll_fun_body))
;;

let lift_binding = function
  | CVal (id, body) ->
    let ll_body, new_funcs = lift_expr [] body in
    let new_funcs_rev = List.rev new_funcs in
    List.rev @@ (LVal (id, ll_body) :: new_funcs_rev)
  | CFun (id, args, body) ->
    let ll_body, new_funcs = lift_expr [] body in
    let new_funcs_rev = List.rev new_funcs in
    List.rev @@ (LFun (id, args, ll_body) :: new_funcs_rev)
;;

let ll_program program =
  reset ();
  List.fold_left program ~f:(fun l r -> l @ lift_binding r) ~init:[]
;;

(* tests *)
let equal ll_program1 ll_program2 = Base.Poly.equal ll_program1 ll_program2

let%test _ =
  let cc = [ CVal ("x", CApp (CIdentifier "f", CLiteral (LInt 3))) ] in
  let ll = [ LVal ("x", LApp (LIdentifier "f", LLiteral (LInt 3))) ] in
  equal (ll_program cc) ll
;;

let%test _ =
  let cc = [ CVal ("x", CBinaryOp (Add, CLiteral (LInt 5), CLiteral (LInt 7))) ] in
  let ll = [ LVal ("x", LBinaryOp (Add, LLiteral (LInt 5), LLiteral (LInt 7))) ] in
  equal (ll_program cc) ll
;;

(* val p = (fn x -> x)

   fun ll_1 x = x
   val p = ll_1
*)

let%test _ =
  let cc =
    [ CVal ("p", CAbs ([ "x" ], CIdentifier "x"))
    ; CVal ("p2", CAbs ([ "x2" ], CIdentifier "x2"))
    ]
  in
  let ll =
    [ LFun ("Ll_1", [ "x" ], LIdentifier "x")
    ; LVal ("p", LIdentifier "Ll_1")
    ; LFun ("Ll_2", [ "x2" ], LIdentifier "x2")
    ; LVal ("p2", LIdentifier "Ll_2")
    ]
  in
  equal (ll_program cc) ll
;;

let%test _ =
  let cc =
    [ CFun
        ( "t"
        , [ "k"; "n"; "m" ]
        , CApp (CIdentifier "k", CApp (CIdentifier "m", CIdentifier "n")) )
    ]
  in
  let ll =
    [ LFun
        ( "t"
        , [ "k"; "n"; "m" ]
        , LApp (LIdentifier "k", LApp (LIdentifier "m", LIdentifier "n")) )
    ]
  in
  equal (ll_program cc) ll
;;

let%test _ =
  let cc =
    [ CFun
        ( "e"
        , [ "n"; "k" ]
        , CIfThenElse
            ( CBinaryOp (LtOrEq, CIdentifier "n", CLiteral (LInt 1))
            , CApp (CIdentifier "k", CLiteral (LInt 1))
            , CApp
                ( CApp
                    (CIdentifier "e", CBinaryOp (Sub, CIdentifier "n", CLiteral (LInt 1)))
                , CIdentifier "n" ) ) )
    ]
  in
  let ll =
    [ LFun
        ( "e"
        , [ "n"; "k" ]
        , LIfThenElse
            ( LBinaryOp (LtOrEq, LIdentifier "n", LLiteral (LInt 1))
            , LApp (LIdentifier "k", LLiteral (LInt 1))
            , LApp
                ( LApp
                    (LIdentifier "e", LBinaryOp (Sub, LIdentifier "n", LLiteral (LInt 1)))
                , LIdentifier "n" ) ) )
    ]
  in
  equal (ll_program cc) ll
;;

let%test _ =
  let cc =
    [ CFun
        ( "j"
        , [ "n" ]
        , CLetIn
            ( CFun ("g", [ "n"; "k" ], CApp (CIdentifier "k", CLiteral (LInt 1)))
            , CApp (CApp (CIdentifier "g", CIdentifier "n"), CLiteral (LInt 1)) ) )
    ]
  in
  let ll =
    [ LFun ("g", [ "n"; "k" ], LApp (LIdentifier "k", LLiteral (LInt 1)))
    ; LFun
        ("j", [ "n" ], LApp (LApp (LIdentifier "g", LIdentifier "n"), LLiteral (LInt 1)))
    ]
  in
  equal (ll_program cc) ll
;;

(*
   fun fac n =
   let fun fack1 k n m = k (m * n)
   in
   let fun fack n k =
   if n <= 1 then k 1
   else fack (n-1) (fack1 k n)
   in
   fack n (fn x => x)
   end
   end

   -> ll ->

   fun fack1 k n m = k (m * n)
   fun fack n k =
   if n <= 1 then k 1
   else fack (n-1) (fack1 k n)
   fun ll_1 x = x
   fun fac n = fack n id
*)
let%test _ =
  let cc =
    [ CFun
        ( "fac"
        , [ "n" ]
        , CLetIn
            ( CFun
                ( "fack1"
                , [ "k"; "n"; "m" ]
                , CApp
                    (CIdentifier "k", CBinaryOp (Mult, CIdentifier "m", CIdentifier "n"))
                )
            , CLetIn
                ( CFun
                    ( "fack"
                    , [ "n"; "k" ]
                    , CIfThenElse
                        ( CBinaryOp (LtOrEq, CIdentifier "n", CLiteral (LInt 1))
                        , CApp (CIdentifier "k", CLiteral (LInt 1))
                        , CApp
                            ( CApp
                                ( CIdentifier "fack"
                                , CBinaryOp (Sub, CIdentifier "n", CLiteral (LInt 1)) )
                            , CApp
                                ( CApp (CIdentifier "fack1", CIdentifier "k")
                                , CIdentifier "n" ) ) ) )
                , CApp
                    ( CApp (CIdentifier "fack", CIdentifier "n")
                    , CAbs ([ "x" ], CIdentifier "x") ) ) ) )
    ]
  in
  let ll =
    [ LFun
        ( "fack1"
        , [ "k"; "n"; "m" ]
        , LApp (LIdentifier "k", LBinaryOp (Mult, LIdentifier "m", LIdentifier "n")) )
    ; LFun
        ( "fack"
        , [ "n"; "k" ]
        , LIfThenElse
            ( LBinaryOp (LtOrEq, LIdentifier "n", LLiteral (LInt 1))
            , LApp (LIdentifier "k", LLiteral (LInt 1))
            , LApp
                ( LApp
                    ( LIdentifier "fack"
                    , LBinaryOp (Sub, LIdentifier "n", LLiteral (LInt 1)) )
                , LApp (LApp (LIdentifier "fack1", LIdentifier "k"), LIdentifier "n") ) )
        )
    ; LFun ("Ll_1", [ "x" ], LIdentifier "x")
    ; LFun
        ( "fac"
        , [ "n" ]
        , LApp (LApp (LIdentifier "fack", LIdentifier "n"), LIdentifier "Ll_1") )
    ]
  in
  equal (ll_program cc) ll
;;
