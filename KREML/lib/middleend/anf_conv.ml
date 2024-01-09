(** Copyright 2023-2024, Anton Kraev and Polina Badreeva *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ll_ast
open Anf
open Counter

let gen_var = gen_var "Anf"

let rec anf (e : ll_expr) (expr_with_hole : immexpr -> aexpr) =
  match e with
  | LLiteral (LInt n) -> expr_with_hole (ImmInt n)
  | LLiteral (LBool n) -> expr_with_hole (ImmBool n)
  | LIdentifier x -> expr_with_hole (ImmIdentifier x)
  | LUnaryOp (op, body) ->
    anf body (fun imm ->
      let varname = gen_var () in
      ALet (varname, CUnaryOp (op, imm), expr_with_hole (ImmIdentifier varname)))
  | LBinaryOp (op, left, right) ->
    anf left (fun limm ->
      anf right (fun rimm ->
        let varname = gen_var () in
        ALet (varname, CBinaryOp (op, limm, rimm), expr_with_hole (ImmIdentifier varname))))
  | LApp (left, right) ->
    anf left (fun limm ->
      anf right (fun rimm ->
        let varname = gen_var () in
        ALet (varname, CApp (limm, rimm), expr_with_hole (ImmIdentifier varname))))
  | LIfThenElse (condition, true_branch, false_branch) ->
    let varname = gen_var () in
    anf condition (fun cond ->
      let t = anf true_branch (fun t_branch -> ACExpr (CImmExpr t_branch)) in
      let f = anf false_branch (fun f_branch -> ACExpr (CImmExpr f_branch)) in
      ALet (varname, CIfThenElse (cond, t, f), expr_with_hole (ImmIdentifier varname)))
  | LLetIn (id, left, right) ->
    anf left (fun limm ->
      anf right (fun rimm -> ALet (id, CImmExpr limm, expr_with_hole rimm)))
;;

let anf_binding = function
  | LVal (id, body) -> AFun (id, [], anf body (fun imm -> ACExpr (CImmExpr imm)))
  | LFun (id, args, body) -> AFun (id, args, anf body (fun imm -> ACExpr (CImmExpr imm)))
;;

let anf_program program =
  reset ();
  List.map anf_binding program
;;

(* tests *)
let equal anf_program1 cc_program2 = Base.Poly.equal anf_program1 cc_program2

(* fack1 k n m = k (m * n)
   fack1 k n m =
   let Anf_1 = m * n in
   let Anf_2 = k Anf_1
   in Anf_2
*)
let%test _ =
  let ll =
    [ LFun
        ( "fack1"
        , [ "k"; "n"; "m" ]
        , LApp (LIdentifier "k", LBinaryOp (Mult, LIdentifier "m", LIdentifier "n")) )
    ; LFun ("ll_1", [ "x" ], LIdentifier "x")
    ; LFun ("fac", [ "n" ], LApp (LIdentifier "n", LIdentifier "ll_1"))
    ]
  in
  let anf =
    [ AFun
        ( "fack1"
        , [ "k"; "n"; "m" ]
        , ALet
            ( "Anf_1"
            , CBinaryOp (Mult, ImmIdentifier "m", ImmIdentifier "n")
            , ALet
                ( "Anf_2"
                , CApp (ImmIdentifier "k", ImmIdentifier "Anf_1")
                , ACExpr (CImmExpr (ImmIdentifier "Anf_2")) ) ) )
    ; AFun ("ll_1", [ "x" ], ACExpr (CImmExpr (ImmIdentifier "x")))
    ; AFun
        ( "fac"
        , [ "n" ]
        , ALet
            ( "Anf_3"
            , CApp (ImmIdentifier "n", ImmIdentifier "ll_1")
            , ACExpr (CImmExpr (ImmIdentifier "Anf_3")) ) )
    ]
  in
  equal (anf_program ll) anf
;;

(*add a b = let Anf_2 = ~a in let Anf_1 = Anf_2 + b in Anf_1*)
let%test _ =
  let ll =
    [ LFun
        ( "add"
        , [ "a"; "b" ]
        , LBinaryOp (Add, LUnaryOp (Neg, LIdentifier "a"), LIdentifier "b") )
    ]
  in
  let anf =
    [ AFun
        ( "add"
        , [ "a"; "b" ]
        , ALet
            ( "Anf_1"
            , CUnaryOp (Neg, ImmIdentifier "a")
            , ALet
                ( "Anf_2"
                , CBinaryOp (Add, ImmIdentifier "Anf_1", ImmIdentifier "b")
                , ACExpr (CImmExpr (ImmIdentifier "Anf_2")) ) ) )
    ]
  in
  equal (anf_program ll) anf
;;

let%test _ =
  let ll =
    [ LFun ("add", [ "a"; "b" ], LBinaryOp (Add, LIdentifier "a", LIdentifier "b")) ]
  in
  let anf =
    [ AFun
        ( "add"
        , [ "a"; "b" ]
        , ALet
            ( "Anf_1"
            , CBinaryOp (Add, ImmIdentifier "a", ImmIdentifier "b")
            , ACExpr (CImmExpr (ImmIdentifier "Anf_1")) ) )
    ]
  in
  equal (anf_program ll) anf
;;

let%test _ =
  let ll = [ LFun ("neg", [ "x" ], LUnaryOp (Neg, LIdentifier "x")) ] in
  let anf =
    [ AFun
        ( "neg"
        , [ "x" ]
        , ALet
            ( "Anf_1"
            , CUnaryOp (Neg, ImmIdentifier "x")
            , ACExpr (CImmExpr (ImmIdentifier "Anf_1")) ) )
    ]
  in
  equal (anf_program ll) anf
;;

let%test _ =
  let ll =
    [ LFun ("foo", [ "x"; "y" ], LBinaryOp (Add, LIdentifier "x", LIdentifier "y"))
    ; LFun
        ( "bar"
        , [ "a"; "b"; "c" ]
        , LApp (LIdentifier "a", LBinaryOp (Sub, LIdentifier "b", LIdentifier "c")) )
    ]
  in
  let anf =
    [ AFun
        ( "foo"
        , [ "x"; "y" ]
        , ALet
            ( "Anf_1"
            , CBinaryOp (Add, ImmIdentifier "x", ImmIdentifier "y")
            , ACExpr (CImmExpr (ImmIdentifier "Anf_1")) ) )
    ; AFun
        ( "bar"
        , [ "a"; "b"; "c" ]
        , ALet
            ( "Anf_2"
            , CBinaryOp (Sub, ImmIdentifier "b", ImmIdentifier "c")
            , ALet
                ( "Anf_3"
                , CApp (ImmIdentifier "a", ImmIdentifier "Anf_2")
                , ACExpr (CImmExpr (ImmIdentifier "Anf_3")) ) ) )
    ]
  in
  equal (anf_program ll) anf
;;

(*
   fun fack1 k n m = k (m * n)
   fun fack n k =
   if n <= 1 then k 1
   else fack (n-1) (fack1 k n)
   fun id x = x
   fun fac n = fack n id

   -> anf  ->

   fun fack1 k n m =
   let Anf_1 = m n in
   let Anf_2 = k Anf_1 in
   in Anf_2;

   fun fack n k =
   let Anf_3 = n <= 1 in
   let Anf_4 = k 1 in
   let Anf_5 = n - 1 in
   let Anf_6 = fack Anf_5 in
   let Anf_7 = fack1 k in
   let Anf_8 = Anf_7 n in
   let Anf_9 =Anf_6 Anf_8 in
   if Anf_3 then Anf_4 else Anf_9;

   fun ll_1 x = x;

   fun n =
   let Anf_10 = fack n in
   let Anf_11 = Anf_10 ll_1
   in Anf_11
*)
let%test _ =
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
    ; LFun ("ll_1", [ "x" ], LIdentifier "x")
    ; LFun
        ( "fac"
        , [ "n" ]
        , LApp (LApp (LIdentifier "fack", LIdentifier "n"), LIdentifier "ll_1") )
    ]
  in
  let anf =
    [ AFun
        ( "fack1"
        , [ "k"; "n"; "m" ]
        , ALet
            ( "Anf_1"
            , CBinaryOp (Mult, ImmIdentifier "m", ImmIdentifier "n")
            , ALet
                ( "Anf_2"
                , CApp (ImmIdentifier "k", ImmIdentifier "Anf_1")
                , ACExpr (CImmExpr (ImmIdentifier "Anf_2")) ) ) )
    ; AFun
        ( "fack"
        , [ "n"; "k" ]
        , ALet
            ( "Anf_4"
            , CBinaryOp (LtOrEq, ImmIdentifier "n", ImmInt 1)
            , ALet
                ( "Anf_3"
                , CIfThenElse
                    ( ImmIdentifier "Anf_4"
                    , ALet
                        ( "Anf_5"
                        , CApp (ImmIdentifier "k", ImmInt 1)
                        , ACExpr (CImmExpr (ImmIdentifier "Anf_5")) )
                    , ALet
                        ( "Anf_6"
                        , CBinaryOp (Sub, ImmIdentifier "n", ImmInt 1)
                        , ALet
                            ( "Anf_7"
                            , CApp (ImmIdentifier "fack", ImmIdentifier "Anf_6")
                            , ALet
                                ( "Anf_8"
                                , CApp (ImmIdentifier "fack1", ImmIdentifier "k")
                                , ALet
                                    ( "Anf_9"
                                    , CApp (ImmIdentifier "Anf_8", ImmIdentifier "n")
                                    , ALet
                                        ( "Anf_10"
                                        , CApp
                                            (ImmIdentifier "Anf_7", ImmIdentifier "Anf_9")
                                        , ACExpr (CImmExpr (ImmIdentifier "Anf_10")) ) )
                                ) ) ) )
                , ACExpr (CImmExpr (ImmIdentifier "Anf_3")) ) ) )
    ; AFun ("ll_1", [ "x" ], ACExpr (CImmExpr (ImmIdentifier "x")))
    ; AFun
        ( "fac"
        , [ "n" ]
        , ALet
            ( "Anf_11"
            , CApp (ImmIdentifier "fack", ImmIdentifier "n")
            , ALet
                ( "Anf_12"
                , CApp (ImmIdentifier "Anf_11", ImmIdentifier "ll_1")
                , ACExpr (CImmExpr (ImmIdentifier "Anf_12")) ) ) )
    ]
  in
  equal (anf_program ll) anf
;;

let%test _ =
  let ll = [ LFun ("f", [ "x" ], LLetIn ("y", LIdentifier "x", LIdentifier "y")) ] in
  let anf =
    [ AFun
        ( "f"
        , [ "x" ]
        , ALet ("y", CImmExpr (ImmIdentifier "x"), ACExpr (CImmExpr (ImmIdentifier "y")))
        )
    ]
  in
  equal (anf_program ll) anf
;;
