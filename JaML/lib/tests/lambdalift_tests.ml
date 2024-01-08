(** Copyright 2023-2024, Ilya Pankratov, Maxim Drumov *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open Jaml_lib.Lambdalift
open Jaml_lib.Typedtree
open Jaml_lib.Toplevel

let print_llstatements program = Format.printf "%s" (show_llstatements program)

let%expect_test _ =
  (* Input:
     let test x =
     let id1 y = y in
     let id2 z = id1 z in
     let id3 w = id2 w in
     let id4 u = id3 u in
     id4 x

     Output:
     let id1 y = y
     let id2 z = id1 z
     let id3 w = id2 w
     let id4 u = id3 u
     let test x = id4 x
  *)
  let _ =
    let e =
      [ TLet
          ( "test"
          , TFun
              ( Arg ("x", Tyvar 12)
              , TLetIn
                  ( "id1"
                  , TFun
                      (Arg ("y", Tyvar 1), TVar ("y", Tyvar 1), Arrow (Tyvar 1, Tyvar 1))
                  , TLetIn
                      ( "id2"
                      , TFun
                          ( Arg ("z", Tyvar 4)
                          , TApp
                              ( TVar ("id1", Arrow (Tyvar 4, Tyvar 4))
                              , TVar ("z", Tyvar 4)
                              , Arrow (Tyvar 4, Tyvar 4) )
                          , Arrow (Tyvar 4, Tyvar 4) )
                      , TLetIn
                          ( "id3"
                          , TFun
                              ( Arg ("w", Tyvar 7)
                              , TApp
                                  ( TVar ("id2", Arrow (Tyvar 7, Tyvar 7))
                                  , TVar ("w", Tyvar 7)
                                  , Arrow (Tyvar 7, Tyvar 7) )
                              , Arrow (Tyvar 7, Tyvar 7) )
                          , TLetIn
                              ( "id4"
                              , TFun
                                  ( Arg ("u", Tyvar 10)
                                  , TApp
                                      ( TVar ("id3", Arrow (Tyvar 10, Tyvar 10))
                                      , TVar ("u", Tyvar 10)
                                      , Arrow (Tyvar 10, Tyvar 10) )
                                  , Arrow (Tyvar 10, Tyvar 10) )
                              , TApp
                                  ( TVar ("id4", Arrow (Tyvar 12, Tyvar 12))
                                  , TVar ("x", Tyvar 12)
                                  , Arrow (Tyvar 12, Tyvar 12) )
                              , Arrow (Tyvar 10, Tyvar 10) )
                          , Arrow (Tyvar 7, Tyvar 7) )
                      , Arrow (Tyvar 4, Tyvar 4) )
                  , Arrow (Tyvar 1, Tyvar 1) )
              , Arrow (Tyvar 12, Tyvar 12) )
          , Arrow (Tyvar 12, Tyvar 12) )
      ]
    in
    lambda_lift e |> print_llstatements
  in
  [%expect
    {|
    [(LLet ("id1", [(Arg ("y", (Tyvar 1)))], (LVar ("y", (Tyvar 1))),
        (Arrow ((Tyvar 1), (Tyvar 1)))));
      (LLet ("id2", [(Arg ("z", (Tyvar 4)))],
         (LApp ((LVar ("id1", (Arrow ((Tyvar 4), (Tyvar 4))))),
            (LVar ("z", (Tyvar 4))), (Arrow ((Tyvar 4), (Tyvar 4))))),
         (Arrow ((Tyvar 4), (Tyvar 4)))));
      (LLet ("id3", [(Arg ("w", (Tyvar 7)))],
         (LApp ((LVar ("id2", (Arrow ((Tyvar 7), (Tyvar 7))))),
            (LVar ("w", (Tyvar 7))), (Arrow ((Tyvar 7), (Tyvar 7))))),
         (Arrow ((Tyvar 7), (Tyvar 7)))));
      (LLet ("id4", [(Arg ("u", (Tyvar 10)))],
         (LApp ((LVar ("id3", (Arrow ((Tyvar 10), (Tyvar 10))))),
            (LVar ("u", (Tyvar 10))), (Arrow ((Tyvar 10), (Tyvar 10))))),
         (Arrow ((Tyvar 10), (Tyvar 10)))));
      (LLet ("test", [(Arg ("x", (Tyvar 12)))],
         (LApp ((LVar ("id4", (Arrow ((Tyvar 12), (Tyvar 12))))),
            (LVar ("x", (Tyvar 12))), (Arrow ((Tyvar 12), (Tyvar 12))))),
         (Arrow ((Tyvar 12), (Tyvar 12)))))
      ]
 |}]
;;

let%expect_test _ =
  (* Input:
     let sum x =
     let new_sum x = x + 1 in
     new_sum x

     Output:
     let new_sum x = x + 1

     let sum x = new_sum x
  *)
  let _ =
    let e =
      [ TLet
          ( "sum"
          , TFun
              ( Arg ("x", Prim Int)
              , TLetIn
                  ( "new_sum"
                  , TFun
                      ( Arg ("x", Prim Int)
                      , TBinop
                          ( Add
                          , TVar ("x", Prim Int)
                          , TConst (CInt 1, Prim Int)
                          , Arrow (Prim Int, Arrow (Prim Int, Prim Int)) )
                      , Arrow (Prim Int, Prim Int) )
                  , TApp
                      ( TVar ("new_sum", Arrow (Prim Int, Prim Int))
                      , TVar ("x", Prim Int)
                      , Prim Int )
                  , Arrow (Prim Int, Prim Int) )
              , Arrow (Prim Int, Prim Int) )
          , Arrow (Prim Int, Prim Int) )
      ]
    in
    lambda_lift e |> print_llstatements
  in
  [%expect
    {|
    [(LLet ("new_sum", [(Arg ("x", (Prim int)))],
        (LBinop (Add, (LVar ("x", (Prim int))), (LConst ((CInt 1), (Prim int))),
           (Arrow ((Prim int), (Arrow ((Prim int), (Prim int))))))),
        (Arrow ((Prim int), (Prim int)))));
      (LLet ("sum", [(Arg ("x", (Prim int)))],
         (LApp ((LVar ("new_sum", (Arrow ((Prim int), (Prim int))))),
            (LVar ("x", (Prim int))), (Prim int))),
         (Arrow ((Prim int), (Prim int)))))
      ]
 |}]
;;

let%expect_test _ =
  (* Input:
     let sum x =
     let var = 1 in
     let new_sum x var = var + x in
     new_sum x var

     Output:
     let new_sum x var = var + x

     let sum x =
     let var = 1 in
     new_sum x var
  *)
  let _ =
    let e =
      [ TLet
          ( "sum"
          , TFun
              ( Arg ("x", Prim Int)
              , TLetIn
                  ( "var"
                  , TConst (CInt 1, Prim Int)
                  , TLetIn
                      ( "new_sum"
                      , TFun
                          ( Arg ("x", Prim Int)
                          , TFun
                              ( Arg ("var", Prim Int)
                              , TBinop
                                  ( Add
                                  , TVar ("var", Prim Int)
                                  , TVar ("x", Prim Int)
                                  , Arrow (Prim Int, Arrow (Prim Int, Prim Int)) )
                              , Arrow (Prim Int, Prim Int) )
                          , Arrow (Prim Int, Arrow (Prim Int, Prim Int)) )
                      , TApp
                          ( TApp
                              ( TVar
                                  ("new_sum", Arrow (Prim Int, Arrow (Prim Int, Prim Int)))
                              , TVar ("x", Prim Int)
                              , Arrow (Prim Int, Prim Int) )
                          , TVar ("var", Prim Int)
                          , Prim Int )
                      , Arrow (Prim Int, Arrow (Prim Int, Prim Int)) )
                  , Prim Int )
              , Arrow (Prim Int, Prim Int) )
          , Arrow (Prim Int, Prim Int) )
      ]
    in
    lambda_lift e |> print_llstatements
  in
  [%expect
    {|
    [(LLet ("new_sum", [(Arg ("x", (Prim int))); (Arg ("var", (Prim int)))],
        (LBinop (Add, (LVar ("var", (Prim int))), (LVar ("x", (Prim int))),
           (Arrow ((Prim int), (Arrow ((Prim int), (Prim int))))))),
        (Arrow ((Prim int), (Arrow ((Prim int), (Prim int)))))));
      (LLet ("sum", [(Arg ("x", (Prim int)))],
         (LLetIn ("var", (LConst ((CInt 1), (Prim int))),
            (LApp (
               (LApp (
                  (LVar ("new_sum",
                     (Arrow ((Prim int), (Arrow ((Prim int), (Prim int))))))),
                  (LVar ("x", (Prim int))), (Arrow ((Prim int), (Prim int))))),
               (LVar ("var", (Prim int))), (Prim int))),
            (Prim int))),
         (Arrow ((Prim int), (Prim int)))))
      ]
 |}]
;;

let%expect_test _ =
  (* Input:
     let sum x =
     let new_x = x + 1 in
     let new_sum = new_x + 1 in
     new_sum

     Output:
     let sum x =
     let new_x = x + 1 in
     let new_sum = new_x + 1 in
     new_sum
  *)
  let _ =
    let e =
      [ TLet
          ( "sum"
          , TFun
              ( Arg ("x", Prim Int)
              , TLetIn
                  ( "new_x"
                  , TBinop
                      ( Add
                      , TVar ("x", Prim Int)
                      , TConst (CInt 1, Prim Int)
                      , Arrow (Prim Int, Arrow (Prim Int, Prim Int)) )
                  , TLetIn
                      ( "new_sum"
                      , TBinop
                          ( Add
                          , TVar ("new_x", Prim Int)
                          , TConst (CInt 1, Prim Int)
                          , Arrow (Prim Int, Arrow (Prim Int, Prim Int)) )
                      , TVar ("new_sum", Prim Int)
                      , Prim Int )
                  , Prim Int )
              , Arrow (Prim Int, Prim Int) )
          , Arrow (Prim Int, Prim Int) )
      ]
    in
    lambda_lift e |> print_llstatements
  in
  [%expect
    {|
    [(LLet ("sum", [(Arg ("x", (Prim int)))],
        (LLetIn ("new_x",
           (LBinop (Add, (LVar ("x", (Prim int))),
              (LConst ((CInt 1), (Prim int))),
              (Arrow ((Prim int), (Arrow ((Prim int), (Prim int))))))),
           (LLetIn ("new_sum",
              (LBinop (Add, (LVar ("new_x", (Prim int))),
                 (LConst ((CInt 1), (Prim int))),
                 (Arrow ((Prim int), (Arrow ((Prim int), (Prim int))))))),
              (LVar ("new_sum", (Prim int))), (Prim int))),
           (Prim int))),
        (Arrow ((Prim int), (Prim int)))))
      ]
 |}]
;;
