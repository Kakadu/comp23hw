(** Copyright 2023-2024, Anton Kraev and Polina Badreeva *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Base
open Ast
open Cc_ast
open Counter

let gen_var = gen_var "Cc"

let simplify program =
  let rec uncurry acc = function
    | EAbs (arg, body) -> uncurry (arg :: acc) body
    | expr -> CAbs (List.rev acc, decompose_letin_expr expr)
  and decompose_letin_expr = function
    | EIdentifier x -> CIdentifier x
    | ELiteral x -> CLiteral x
    | EUnaryOp (op, body) -> CUnaryOp (op, decompose_letin_expr body)
    | EBinaryOp (op, l, r) ->
      CBinaryOp (op, decompose_letin_expr l, decompose_letin_expr r)
    | EIfThenElse (c, t, e) ->
      CIfThenElse (decompose_letin_expr c, decompose_letin_expr t, decompose_letin_expr e)
    | EApp (l, r) -> CApp (decompose_letin_expr l, decompose_letin_expr r)
    | EAbs (arg, body) -> uncurry [ arg ] body
    | ELetIn (bindings, body) ->
      List.fold_right
        bindings
        ~f:(fun b1 b2 -> CLetIn (decompose_letin_bind b1, b2))
        ~init:(decompose_letin_expr body)
  and decompose_letin_bind = function
    | BVal (name, body) -> CVal (name, decompose_letin_expr body)
    | BFun (name, args, body) -> CFun (name, args, decompose_letin_expr body)
  in
  List.map program ~f:decompose_letin_bind
;;

let rec free_vars = function
  | CIdentifier var -> Set.singleton (module String) var
  | CUnaryOp (_, body) -> free_vars body
  | CBinaryOp (_, left, right) | CApp (left, right) ->
    Set.union (free_vars left) (free_vars right)
  | CIfThenElse (cond, if_true, if_false) ->
    Set.union_list
      (module String)
      [ free_vars cond; free_vars if_true; free_vars if_false ]
  | _ -> Set.empty (module String)
;;

let create_closure closed_args args body =
  let name = gen_var () in
  CFun (name, List.append closed_args args, body)
;;

let apply_closure closure args =
  let id =
    match closure with
    | CFun (varname, _, _) | CVal (varname, _) -> varname
  in
  List.fold_left args ~f:(fun l r -> CApp (l, CIdentifier r)) ~init:(CIdentifier id)
;;

let get_function_names env =
  let get_name = function
    | CFun (varname, _, _) | CVal (varname, _) -> varname
  in
  Set.of_list (module String) (List.map env ~f:get_name)
;;


let replace_id_by_expr where id expr =
  let rec helper = function
    | CIdentifier x when equal_string id x -> expr
    | CUnaryOp (op, b) -> CUnaryOp (op, helper b)
    | CBinaryOp (op, l, r) -> CBinaryOp (op, helper l, helper r)
    | CAbs (l, r) -> CAbs (l, helper r)
    | CApp (l, r) -> CApp (helper l, helper r)
    | CIfThenElse (cond, if_true, if_false) ->
      CIfThenElse (helper cond, helper if_true, helper if_false)
    | CLetIn (binding, letin_body) ->
      let new_binding =
        match binding with
        | CVal (id, body) -> CVal (id, helper body)
        | CFun (id, args, body) -> CFun (id, args, helper body)
      in
      CLetIn (new_binding, helper letin_body)
    | e -> e
  in
  helper where
;;

let rec clos_conv env parent_scope = function
  | CIdentifier x -> CIdentifier x, env
  | CLiteral x -> CLiteral x, env
  | CUnaryOp (op, body) ->
    let cc_body, env' = clos_conv env parent_scope body in
    CUnaryOp (op, cc_body), env'
  | CBinaryOp (op, left, right) ->
    let cc_left, env' = clos_conv env parent_scope left in
    let cc_right, env'' = clos_conv env' parent_scope right in
    CBinaryOp (op, cc_left, cc_right), env''
  | CIfThenElse (cond, if_true, if_false) ->
    let cc_cond, env' = clos_conv env parent_scope cond in
    let cc_if_true, env'' = clos_conv env' parent_scope if_true in
    let cc_if_false, env''' = clos_conv env'' parent_scope if_false in
    CIfThenElse (cc_cond, cc_if_true, cc_if_false), env'''
  | CApp (left, right) ->
    let cc_left, env' = clos_conv env parent_scope left in
    let cc_right, env'' = clos_conv env' parent_scope right in
    CApp (cc_left, cc_right), env''
  | CAbs (args, body) ->
    let cc_body, env' = clos_conv env parent_scope body in
    let closed_args =
      Set.diff
        (free_vars cc_body)
        (Set.union
           (get_function_names env')
           (Set.of_list (module String) (args @ parent_scope)))
    in
    let cc_abs, env'' =
      match Set.to_list closed_args with
      | [] -> CAbs (args, cc_body), env'
      | closed_args ->
        let closure = create_closure closed_args args cc_body in
        apply_closure closure closed_args, closure :: env'
    in
    cc_abs, env''
  | CLetIn (binding, letin_body) ->
    (match binding with
     | CVal (id, body) ->
       let cc_body, env' = clos_conv env parent_scope body in
       let cc_letin_body, env'' = clos_conv env' (id :: parent_scope) letin_body in
       CLetIn (CVal (id, cc_body), cc_letin_body), env''
     | CFun (id, args, body) ->
       let cc_body, env' = clos_conv env parent_scope body in
       let closed_args =
         Set.diff
           (free_vars cc_body)
           (Set.union
              (get_function_names env')
              (Set.of_list (module String) ((id :: args) @ parent_scope)))
       in
       (match Set.to_list closed_args with
        | [] ->
          let cc_letin_body, env'' = clos_conv env' (id :: parent_scope) letin_body in
          CLetIn (CFun (id, args, cc_body), cc_letin_body), env''
        | closed_args ->
          let closure = CFun (id, closed_args @ args, cc_body) in
          let applied_closure = apply_closure closure closed_args in
          let cc_body' = replace_id_by_expr cc_body id applied_closure in
          let cc_letin_body = replace_id_by_expr letin_body id applied_closure in
          let cc_letin_body', env'' = clos_conv env' (id :: parent_scope) cc_letin_body in
          cc_letin_body', CFun (id, closed_args @ args, cc_body') :: env''))
;;

let get_name = function
  | CVal (id, _) | CFun (id, _, _) -> id
;;

let cc_program program =
  reset ();
  let rec helper acc parent_scope = function
    | [] -> acc
    | CVal (id, body) :: tl ->
      let cc_body, env = clos_conv [] parent_scope body in
      helper
        (CVal (id, List.fold_right env ~f:(fun l r -> CLetIn (l, r)) ~init:cc_body) :: acc)
        (id :: parent_scope)
        tl
    | CFun (id, args, body) :: tl ->
      let cc_body, env = clos_conv [] (id :: parent_scope) body in
      helper
        (CFun (id, args, List.fold_right env ~f:(fun l r -> CLetIn (l, r)) ~init:cc_body)
         :: acc)
        (id :: parent_scope)
        tl
  in
  List.rev (helper [] [] (simplify program))
;;

(* tests *)
let equal anf_program1 cc_program2 = Base.Poly.equal anf_program1 cc_program2

let%test _ =
  let ast =
    [ BVal
        ( "x"
        , EIfThenElse
            ( EBinaryOp (GtOrEq, EIdentifier "y", ELiteral (LInt 4))
            , EAbs ("z", EBinaryOp (Add, EIdentifier "z", ELiteral (LInt 1)))
            , EIdentifier "f" ) )
    ]
  in
  let cc =
    [ CVal
        ( "x"
        , CIfThenElse
            ( CBinaryOp (GtOrEq, CIdentifier "y", CLiteral (LInt 4))
            , CAbs ([ "z" ], CBinaryOp (Add, CIdentifier "z", CLiteral (LInt 1)))
            , CIdentifier "f" ) )
    ]
  in
  equal (cc_program ast) cc
;;

let%test _ =
  let ast =
    [ BVal
        ( "x"
        , EBinaryOp
            (And, EBinaryOp (Or, EIdentifier "a", EIdentifier "b"), EIdentifier "c") )
    ]
  in
  let cc =
    [ CVal
        ( "x"
        , CBinaryOp
            (And, CBinaryOp (Or, CIdentifier "a", CIdentifier "b"), CIdentifier "c") )
    ]
  in
  equal (cc_program ast) cc
;;

(*
   fun fac n =
   let fun fack n k =
   if n <= 1 then k 1
   else fack (n-1) (fn m => k (m * n))
   in
   fack n (fn x => x)
   end

   -> cc ->

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
*)

let%test _ =
  let ast =
    [ BFun
        ( "fac"
        , [ "n" ]
        , ELetIn
            ( [ BFun
                  ( "fack"
                  , [ "n"; "k" ]
                  , EIfThenElse
                      ( EBinaryOp (LtOrEq, EIdentifier "n", ELiteral (LInt 1))
                      , EApp (EIdentifier "k", ELiteral (LInt 1))
                      , EApp
                          ( EApp
                              ( EIdentifier "fack"
                              , EBinaryOp (Sub, EIdentifier "n", ELiteral (LInt 1)) )
                          , EAbs
                              ( "m"
                              , EApp
                                  ( EIdentifier "k"
                                  , EBinaryOp (Mult, EIdentifier "m", EIdentifier "n") )
                              ) ) ) )
              ]
            , EApp
                (EApp (EIdentifier "fack", EIdentifier "n"), EAbs ("x", EIdentifier "x"))
            ) )
    ]
  in
  let cc =
    [ CFun
        ( "fac"
        , [ "n" ]
        , CLetIn
            ( CFun
                ( "Cc_1"
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
                                ( CApp (CIdentifier "Cc_1", CIdentifier "k")
                                , CIdentifier "n" ) ) ) )
                , CApp
                    ( CApp (CIdentifier "fack", CIdentifier "n")
                    , CAbs ([ "x" ], CIdentifier "x") ) ) ) )
    ]
  in
  equal (cc_program ast) cc
;;

(*
   fun f1 x = x
   fun f2 m =
   let
   fun f3 y = m
   in (fun z -> f1 f2 f3 m + z) end

   -> cc ->

   fun f1 x = x
   fun f2 m =
   let fun f3 m y = m in
   let fun cc_1 m z = f1 f2 (f3 m) m + z in
   cc_1 m end
*)

let%test _ =
  let ast =
    [ BFun ("f1", [ "x" ], EIdentifier "x")
    ; BFun
        ( "f2"
        , [ "m" ]
        , ELetIn
            ( [ BFun ("f3", [ "y" ], EIdentifier "m") ]
            , EAbs
                ( "z"
                , EBinaryOp
                    ( Add
                    , EApp
                        ( EApp
                            (EApp (EIdentifier "f1", EIdentifier "f2"), EIdentifier "f3")
                        , EIdentifier "m" )
                    , EIdentifier "z" ) ) ) )
    ]
  in
  let cc =
    [ CFun ("f1", [ "x" ], CIdentifier "x")
    ; CFun
        ( "f2"
        , [ "m" ]
        , CLetIn
            ( CFun ("f3", [ "m"; "y" ], CIdentifier "m")
            , CLetIn
                ( CFun
                    ( "Cc_1"
                    , [ "m"; "z" ]
                    , CBinaryOp
                        ( Add
                        , CApp
                            ( CApp
                                ( CApp (CIdentifier "f1", CIdentifier "f2")
                                , CApp (CIdentifier "f3", CIdentifier "m") )
                            , CIdentifier "m" )
                        , CIdentifier "z" ) )
                , CApp (CIdentifier "Cc_1", CIdentifier "m") ) ) )
    ]
  in
  equal (cc_program ast) cc
;;
