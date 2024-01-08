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
  (* Input:
     let sum x =
     let new_sum y = x + y in
     new_sum 5

     Output:
     let sum x =
     let new_sum x y = x + y in
     new_sum x 5
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
                      ( Arg ("y", Prim Int)
                      , TBinop
                          ( Add
                          , TVar ("x", Prim Int)
                          , TVar ("y", Prim Int)
                          , Arrow (Prim Int, Arrow (Prim Int, Prim Int)) )
                      , Arrow (Prim Int, Prim Int) )
                  , TApp
                      ( TVar ("new_sum", Arrow (Prim Int, Prim Int))
                      , TConst (CInt 5, Prim Int)
                      , Prim Int )
                  , Arrow (Prim Int, Prim Int) )
              , Arrow (Prim Int, Prim Int) )
          , Arrow (Prim Int, Prim Int) )
      ]
    in
    Closure.closure e |> pp_with_type_statements
  in
  [%expect
    {|
    (TLet(
        sum: (int -> int),
        (TFun: (int -> int) (
            (x: int),
            (TLetIn(
                new_sum: (int -> (int -> int)),
                (TFun: (int -> (int -> int)) (
                    (x: int),
                    (TFun: (int -> int) (
                        (y: int),
                        (Add: (int -> (int -> int)) (
                            (x: int),
                            (y: int)
                        ))
                    ))
                )),
                (TApp: int (
                    (TApp: (int -> (int -> (int -> int))) (
                        (new_sum: (int -> int)),
                        (x: int)
                    )),
                    (TConst((CInt 5): int))
                ))
            ))
        ))
    ))
 |}]
;;

let%expect_test _ =
  (* Input:
     let x y =
     let z a = a (y + 1) in
     z (fun x -> x)

     Output:
     let x y =
     let #closure_fun1 x = x in
     let z y a = a (y + 1) in
     z y #closure_fun1
  *)
  let _ =
    let e =
      [ TLet
          ( "x"
          , TFun
              ( Arg ("y", Prim Int)
              , TLetIn
                  ( "z"
                  , TFun
                      ( Arg ("a", Arrow (Prim Int, Tyvar 2))
                      , TApp
                          ( TVar ("a", Arrow (Prim Int, Tyvar 2))
                          , TBinop
                              ( Add
                              , TVar ("y", Prim Int)
                              , TConst (CInt 1, Prim Int)
                              , Arrow (Prim Int, Arrow (Prim Int, Prim Int)) )
                          , Tyvar 2 )
                      , Arrow (Arrow (Prim Int, Tyvar 2), Tyvar 2) )
                  , TApp
                      ( TVar ("z", Arrow (Arrow (Prim Int, Prim Int), Prim Int))
                      , TFun
                          ( Arg ("x", Prim Int)
                          , TVar ("x", Prim Int)
                          , Arrow (Prim Int, Prim Int) )
                      , Prim Int )
                  , Arrow (Arrow (Prim Int, Tyvar 2), Tyvar 2) )
              , Arrow (Prim Int, Prim Int) )
          , Arrow (Prim Int, Prim Int) )
      ]
    in
    Closure.closure e |> pp_with_type_statements
  in
  [%expect
    {|
    (TLet(
        x: (int -> int),
        (TFun: (int -> int) (
            (y: int),
            (TLetIn(
                #closure_fun1: (int -> int),
                (TFun: (int -> int) (
                    (x: int),
                    (x: int)
                )),
                (TLetIn(
                    z: (int -> ((int -> 'a) -> 'a)),
                    (TFun: (int -> ((int -> 'a) -> 'a)) (
                        (y: int),
                        (TFun: ((int -> 'a) -> 'a) (
                            (a: (int -> 'a)),
                            (TApp: (int -> 'a) (
                                (a: (int -> 'a)),
                                (Add: (int -> (int -> int)) (
                                    (y: int),
                                    (TConst((CInt 1): int))
                                ))
                            ))
                        ))
                    )),
                    (TApp: int (
                        (TApp: (int -> (int -> ((int -> 'a) -> 'a))) (
                            (z: ((int -> int) -> int)),
                            (y: int)
                        )),
                        (#closure_fun1: (int -> int))
                    ))
                ))
            ))
        ))
    ))
 |}]
;;

let%expect_test _ =
  (* Input:
     let fac n =
     let rec fack n k =
     if n<=1 then k 1
     else fack (n−1) ((fun k n m −> k (m ∗ n)) k n)
     in
     fack n (fun x −> x)

     Output:
     let fac n =
     let #closure_fun2 k n m = k (m * n)
     let #closure_fun3 x = x in
     let rec fack n k =
     if n<=1 then k 1
     else fack (n−1) (#closure_fun2 k n)
     in
     fack n #closure_fun3
  *)
  let _ =
    let e =
      [ TLet
          ( "fac"
          , TFun
              ( Arg ("n", Prim Int)
              , TLetRecIn
                  ( "fack"
                  , TFun
                      ( Arg ("n", Prim Int)
                      , TFun
                          ( Arg ("k", Arrow (Prim Int, Prim Int))
                          , TIfThenElse
                              ( TBinop
                                  ( Lte
                                  , TVar ("n", Prim Int)
                                  , TConst (CInt 1, Prim Int)
                                  , Arrow (Prim Int, Arrow (Prim Int, Prim Bool)) )
                              , TApp
                                  ( TVar ("k", Arrow (Prim Int, Prim Int))
                                  , TConst (CInt 1, Prim Int)
                                  , Prim Int )
                              , TApp
                                  ( TApp
                                      ( TVar
                                          ( "fack"
                                          , Arrow
                                              ( Prim Int
                                              , Arrow
                                                  (Arrow (Prim Int, Prim Int), Prim Int)
                                              ) )
                                      , TBinop
                                          ( Sub
                                          , TVar ("n", Prim Int)
                                          , TConst (CInt 1, Prim Int)
                                          , Arrow (Prim Int, Arrow (Prim Int, Prim Int))
                                          )
                                      , Arrow (Arrow (Prim Int, Prim Int), Prim Int) )
                                  , TApp
                                      ( TApp
                                          ( TFun
                                              ( Arg ("k", Arrow (Prim Int, Prim Int))
                                              , TFun
                                                  ( Arg ("n", Prim Int)
                                                  , TFun
                                                      ( Arg ("m", Prim Int)
                                                      , TApp
                                                          ( TVar
                                                              ( "k"
                                                              , Arrow (Prim Int, Prim Int)
                                                              )
                                                          , TBinop
                                                              ( Mul
                                                              , TVar ("m", Prim Int)
                                                              , TVar ("n", Prim Int)
                                                              , Arrow
                                                                  ( Prim Int
                                                                  , Arrow
                                                                      (Prim Int, Prim Int)
                                                                  ) )
                                                          , Prim Int )
                                                      , Arrow (Prim Int, Prim Int) )
                                                  , Arrow
                                                      ( Prim Int
                                                      , Arrow (Prim Int, Prim Int) ) )
                                              , Arrow
                                                  ( Arrow (Prim Int, Prim Int)
                                                  , Arrow
                                                      ( Prim Int
                                                      , Arrow (Prim Int, Prim Int) ) ) )
                                          , TVar ("k", Arrow (Prim Int, Prim Int))
                                          , Arrow (Prim Int, Arrow (Prim Int, Prim Int))
                                          )
                                      , TVar ("n", Prim Int)
                                      , Arrow (Prim Int, Prim Int) )
                                  , Prim Int )
                              , Prim Int )
                          , Arrow (Arrow (Prim Int, Prim Int), Prim Int) )
                      , Arrow (Prim Int, Arrow (Arrow (Prim Int, Prim Int), Prim Int)) )
                  , TApp
                      ( TApp
                          ( TVar
                              ( "fack"
                              , Arrow
                                  (Prim Int, Arrow (Arrow (Prim Int, Prim Int), Prim Int))
                              )
                          , TVar ("n", Prim Int)
                          , Arrow (Arrow (Prim Int, Prim Int), Prim Int) )
                      , TFun
                          ( Arg ("x", Prim Int)
                          , TVar ("x", Prim Int)
                          , Arrow (Prim Int, Prim Int) )
                      , Prim Int )
                  , Arrow (Prim Int, Arrow (Arrow (Prim Int, Prim Int), Prim Int)) )
              , Arrow (Prim Int, Prim Int) )
          , Arrow (Prim Int, Prim Int) )
      ]
    in
    Closure.closure e |> pp_with_type_statements
  in
  [%expect
    {|
    (TLet(
        fac: (int -> int),
        (TFun: (int -> int) (
            (n: int),
            (TLetIn(
                #closure_fun2: ((int -> int) -> (int -> (int -> int))),
                (TFun: ((int -> int) -> (int -> (int -> int))) (
                    (k: (int -> int)),
                    (TFun: (int -> (int -> int)) (
                        (n: int),
                        (TFun: (int -> int) (
                            (m: int),
                            (TApp: (int -> int) (
                                (k: (int -> int)),
                                (Mul: (int -> (int -> int)) (
                                    (m: int),
                                    (n: int)
                                ))
                            ))
                        ))
                    ))
                )),
                (TLetIn(
                    #closure_fun3: (int -> int),
                    (TFun: (int -> int) (
                        (x: int),
                        (x: int)
                    )),
                    (TLetRecIn(
                        fack: (int -> ((int -> int) -> int)),
                        (TFun: (int -> ((int -> int) -> int)) (
                            (n: int),
                            (TFun: ((int -> int) -> int) (
                                (k: (int -> int)),
                                (TIfThenElse: int
                                    ((Lte: (int -> (int -> bool)) (
                                        (n: int),
                                        (TConst((CInt 1): int))
                                    )),
                                    (TApp: (int -> int) (
                                        (k: (int -> int)),
                                        (TConst((CInt 1): int))
                                    )),
                                    (TApp: int (
                                        (TApp: (int -> ((int -> int) -> int)) (
                                            (fack: (int -> ((int -> int) -> int))),
                                            (Sub: (int -> (int -> int)) (
                                                (n: int),
                                                (TConst((CInt 1): int))
                                            ))
                                        )),
                                        (TApp: (int -> int) (
                                            (TApp: ((int -> int) -> (int -> (int -> int))) (
                                                (#closure_fun2: ((int -> int) -> (int -> (int -> int)))),
                                                (k: (int -> int))
                                            )),
                                            (n: int)
                                        ))
                                    ))
                                ))
                            ))
                        )),
                        (TApp: int (
                            (TApp: (int -> ((int -> int) -> int)) (
                                (fack: (int -> ((int -> int) -> int))),
                                (n: int)
                            )),
                            (#closure_fun3: (int -> int))
                        ))
                    ))
                ))
            ))
        ))
    ))
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
    Closure.closure e |> pp_with_type_statements
  in
  [%expect
    {|
    (TLet(
        sum: (int -> int),
        (TFun: (int -> int) (
            (x: int),
            (TLetIn(
                new_x: int,
                (Add: (int -> (int -> int)) (
                    (x: int),
                    (TConst((CInt 1): int))
                )),
                (TLetIn(
                    new_sum: int,
                    (Add: (int -> (int -> int)) (
                        (new_x: int),
                        (TConst((CInt 1): int))
                    )),
                    (new_sum: int)
                ))
            ))
        ))
    ))
 |}]
;;

let%expect_test _ =
  (* Input:
     let fibo n =
     let rec fibo_cps n acc =
     if n < 3
     then acc 1
     else fibo_cps (n - 1) (fun x -> fibo_cps (n - 2) (fun y -> acc (x + y)))
     in
     fibo_cps n (fun x -> x)

     Output:
     let fibo n =
     let #closure_fun4 x acc y = acc (x + y) in
     let #closure_fun5 n fibo_cps acc x = fibo_cps (n - 2) (#closure_fun4 x acc) in
     let #closure_fun6 x = x in
     let rec fibo_cps n acc =
     if n < 3
     then acc 1
     else fibo_cps (n - 1) #closure_fun5 n fibo_cps acc
     in
     fibo_cps n #closure_fun6
  *)
  let _ =
    let e =
      [ TLet
          ( "fibo"
          , TFun
              ( Arg ("n", Prim Int)
              , TLetRecIn
                  ( "fibo_cps"
                  , TFun
                      ( Arg ("n", Prim Int)
                      , TFun
                          ( Arg ("acc", Arrow (Prim Int, Prim Int))
                          , TIfThenElse
                              ( TBinop
                                  ( Lt
                                  , TVar ("n", Prim Int)
                                  , TConst (CInt 3, Prim Int)
                                  , Arrow (Prim Int, Arrow (Prim Int, Prim Bool)) )
                              , TApp
                                  ( TVar ("acc", Arrow (Prim Int, Prim Int))
                                  , TConst (CInt 1, Prim Int)
                                  , Prim Int )
                              , TApp
                                  ( TApp
                                      ( TVar
                                          ( "fibo_cps"
                                          , Arrow
                                              ( Prim Int
                                              , Arrow
                                                  (Arrow (Prim Int, Prim Int), Prim Int)
                                              ) )
                                      , TBinop
                                          ( Sub
                                          , TVar ("n", Prim Int)
                                          , TConst (CInt 1, Prim Int)
                                          , Arrow (Prim Int, Arrow (Prim Int, Prim Int))
                                          )
                                      , Arrow (Arrow (Prim Int, Prim Int), Prim Int) )
                                  , TFun
                                      ( Arg ("x", Prim Int)
                                      , TApp
                                          ( TApp
                                              ( TVar
                                                  ( "fibo_cps"
                                                  , Arrow
                                                      ( Prim Int
                                                      , Arrow
                                                          ( Arrow (Prim Int, Prim Int)
                                                          , Prim Int ) ) )
                                              , TBinop
                                                  ( Sub
                                                  , TVar ("n", Prim Int)
                                                  , TConst (CInt 2, Prim Int)
                                                  , Arrow
                                                      ( Prim Int
                                                      , Arrow (Prim Int, Prim Int) ) )
                                              , Arrow
                                                  (Arrow (Prim Int, Prim Int), Prim Int)
                                              )
                                          , TFun
                                              ( Arg ("y", Prim Int)
                                              , TApp
                                                  ( TVar
                                                      ("acc", Arrow (Prim Int, Prim Int))
                                                  , TBinop
                                                      ( Add
                                                      , TVar ("x", Prim Int)
                                                      , TVar ("y", Prim Int)
                                                      , Arrow
                                                          ( Prim Int
                                                          , Arrow (Prim Int, Prim Int) )
                                                      )
                                                  , Prim Int )
                                              , Arrow (Prim Int, Prim Int) )
                                          , Prim Int )
                                      , Arrow (Prim Int, Prim Int) )
                                  , Prim Int )
                              , Prim Int )
                          , Arrow (Arrow (Prim Int, Prim Int), Prim Int) )
                      , Arrow (Prim Int, Arrow (Arrow (Prim Int, Prim Int), Prim Int)) )
                  , TApp
                      ( TApp
                          ( TVar
                              ( "fibo_cps"
                              , Arrow
                                  (Prim Int, Arrow (Arrow (Prim Int, Prim Int), Prim Int))
                              )
                          , TVar ("n", Prim Int)
                          , Arrow (Arrow (Prim Int, Prim Int), Prim Int) )
                      , TFun
                          ( Arg ("x", Prim Int)
                          , TVar ("x", Prim Int)
                          , Arrow (Prim Int, Prim Int) )
                      , Prim Int )
                  , Arrow (Prim Int, Arrow (Arrow (Prim Int, Prim Int), Prim Int)) )
              , Arrow (Prim Int, Prim Int) )
          , Arrow (Prim Int, Prim Int) )
      ]
    in
    Closure.closure e |> pp_with_type_statements
  in
  [%expect
    {|
    (TLet(
        fibo: (int -> int),
        (TFun: (int -> int) (
            (n: int),
            (TLetIn(
                #closure_fun4: (int -> ((int -> int) -> (int -> int))),
                (TFun: (int -> ((int -> int) -> (int -> int))) (
                    (x: int),
                    (TFun: ((int -> int) -> (int -> int)) (
                        (acc: (int -> int)),
                        (TFun: (int -> int) (
                            (y: int),
                            (TApp: (int -> int) (
                                (acc: (int -> int)),
                                (Add: (int -> (int -> int)) (
                                    (x: int),
                                    (y: int)
                                ))
                            ))
                        ))
                    ))
                )),
                (TLetIn(
                    #closure_fun5: (int -> ((int -> ((int -> int) -> int)) -> ((int -> int) -> (int -> int)))),
                    (TFun: (int -> ((int -> ((int -> int) -> int)) -> ((int -> int) -> (int -> int)))) (
                        (n: int),
                        (TFun: ((int -> ((int -> int) -> int)) -> ((int -> int) -> (int -> int))) (
                            (fibo_cps: (int -> ((int -> int) -> int))),
                            (TFun: ((int -> int) -> (int -> int)) (
                                (acc: (int -> int)),
                                (TFun: (int -> int) (
                                    (x: int),
                                    (TApp: int (
                                        (TApp: (int -> ((int -> int) -> int)) (
                                            (fibo_cps: (int -> ((int -> int) -> int))),
                                            (Sub: (int -> (int -> int)) (
                                                (n: int),
                                                (TConst((CInt 2): int))
                                            ))
                                        )),
                                        (TApp: (int -> ((int -> int) -> (int -> int))) (
                                            (TApp: ((int -> int) -> (int -> int)) (
                                                (#closure_fun4: (int -> ((int -> int) -> (int -> int)))),
                                                (acc: (int -> int))
                                            )),
                                            (x: int)
                                        ))
                                    ))
                                ))
                            ))
                        ))
                    )),
                    (TLetIn(
                        #closure_fun6: (int -> int),
                        (TFun: (int -> int) (
                            (x: int),
                            (x: int)
                        )),
                        (TLetRecIn(
                            fibo_cps: (int -> ((int -> int) -> int)),
                            (TFun: (int -> ((int -> int) -> int)) (
                                (n: int),
                                (TFun: ((int -> int) -> int) (
                                    (acc: (int -> int)),
                                    (TIfThenElse: int
                                        ((Lt: (int -> (int -> bool)) (
                                            (n: int),
                                            (TConst((CInt 3): int))
                                        )),
                                        (TApp: (int -> int) (
                                            (acc: (int -> int)),
                                            (TConst((CInt 1): int))
                                        )),
                                        (TApp: int (
                                            (TApp: (int -> ((int -> int) -> int)) (
                                                (fibo_cps: (int -> ((int -> int) -> int))),
                                                (Sub: (int -> (int -> int)) (
                                                    (n: int),
                                                    (TConst((CInt 1): int))
                                                ))
                                            )),
                                            (TApp: (int -> ((int -> ((int -> int) -> int)) -> ((int -> int) -> (int -> int)))) (
                                                (TApp: ((int -> ((int -> int) -> int)) -> ((int -> int) -> (int -> int))) (
                                                    (TApp: ((int -> int) -> (int -> int)) (
                                                        (#closure_fun5: (int -> ((int -> ((int -> int) -> int)) -> ((int -> int) -> (int -> int))))),
                                                        (acc: (int -> int))
                                                    )),
                                                    (fibo_cps: (int -> ((int -> int) -> int)))
                                                )),
                                                (n: int)
                                            ))
                                        ))
                                    ))
                                ))
                            )),
                            (TApp: int (
                                (TApp: (int -> ((int -> int) -> int)) (
                                    (fibo_cps: (int -> ((int -> int) -> int))),
                                    (n: int)
                                )),
                                (#closure_fun6: (int -> int))
                            ))
                        ))
                    ))
                ))
            ))
        ))
    ))
 |}]
;;
