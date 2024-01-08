(** Copyright 2023-2024, Ilya Pankratov, Maxim Drumov *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open Jaml_lib
open Jaml_lib.Typedtree

let pp_with_type_statements te =
  let open Pprinttypedtree in
  let pp_statements = pp_statements ";\n" Complete in
  Format.printf "%a%!" pp_statements te
;;

let%expect_test _ =
  (*
     Input: let name x y = x + y
     Output: let name1 x2 y3 = x2 + y3
  *)
  let _ =
    let e =
      [ TLet
          ( "name"
          , TFun
              ( Arg ("x", Prim Int)
              , TFun
                  ( Arg ("y", Prim Int)
                  , TBinop
                      ( Add
                      , TVar ("x", Prim Int)
                      , TVar ("y", Prim Int)
                      , Arrow (Prim Int, Arrow (Prim Int, Prim Int)) )
                  , Arrow (Prim Int, Prim Int) )
              , Arrow (Prim Int, Arrow (Prim Int, Prim Int)) )
          , Arrow (Prim Int, Arrow (Prim Int, Prim Int)) )
      ]
    in
    Alpha.alpha e |> pp_with_type_statements
  in
  [%expect
    {|
    (TLet(
        name1: (int -> (int -> int)),
        (TFun: (int -> (int -> int)) (
            (x2: int),
            (TFun: (int -> int) (
                (y3: int),
                (Add: (int -> (int -> int)) (
                    (x2: int),
                    (y3: int)
                ))
            ))
        ))
    ))
 |}]
;;

let%expect_test _ =
  (*
     Input:
     let test x y =
     let x = x * x in
     let y = y * y in
     x * y

     Output:
     let test1 x2 y3 =
     let x4 = x2 * x2 in
     let y5 = y3 * y3 in
     x4 * y5
  *)
  let _ =
    let e =
      [ TLet
          ( "test"
          , TFun
              ( Arg ("x", Prim Int)
              , TFun
                  ( Arg ("y", Prim Int)
                  , TLetIn
                      ( "x"
                      , TBinop
                          ( Mul
                          , TVar ("x", Prim Int)
                          , TVar ("x", Prim Int)
                          , Arrow (Prim Int, Arrow (Prim Int, Prim Int)) )
                      , TLetIn
                          ( "y"
                          , TBinop
                              ( Mul
                              , TVar ("y", Prim Int)
                              , TVar ("y", Prim Int)
                              , Arrow (Prim Int, Arrow (Prim Int, Prim Int)) )
                          , TBinop
                              ( Mul
                              , TVar ("x", Prim Int)
                              , TVar ("y", Prim Int)
                              , Arrow (Prim Int, Arrow (Prim Int, Prim Int)) )
                          , Prim Int )
                      , Prim Int )
                  , Arrow (Prim Int, Prim Int) )
              , Arrow (Prim Int, Arrow (Prim Int, Prim Int)) )
          , Arrow (Prim Int, Arrow (Prim Int, Prim Int)) )
      ]
    in
    Alpha.alpha e |> pp_with_type_statements
  in
  [%expect
    {|
    (TLet(
        test1: (int -> (int -> int)),
        (TFun: (int -> (int -> int)) (
            (x2: int),
            (TFun: (int -> int) (
                (y3: int),
                (TLetIn(
                    x4: int,
                    (Mul: (int -> (int -> int)) (
                        (x2: int),
                        (x2: int)
                    )),
                    (TLetIn(
                        y5: int,
                        (Mul: (int -> (int -> int)) (
                            (y3: int),
                            (y3: int)
                        )),
                        (Mul: (int -> (int -> int)) (
                            (x4: int),
                            (y5: int)
                        ))
                    ))
                ))
            ))
        ))
    ))
 |}]
;;
