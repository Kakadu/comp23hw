(** Copyright 2023-2024, Ilya Pankratov, Maxim Drumov *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open Jaml_lib.Anf
open Jaml_lib.Anfconv
open Jaml_lib.Toplevel

let print_anfstatements res = Format.print_string (show_anfstatements res)

let%expect_test _ =
  (*
     Input:
     let x = (1 + 2) * 3

     Output:
     let x =
     let #binop1 = 1 + 2 in
     let #binop2 = binop1 + 3 in
     binop2
  *)
  let _ =
    let e =
      [ LLet
          ( "x"
          , []
          , LBinop
              ( Mul
              , LBinop
                  ( Add
                  , LConst (CInt 1, Prim Int)
                  , LConst (CInt 2, Prim Int)
                  , Arrow (Prim Int, Arrow (Prim Int, Prim Int)) )
              , LConst (CInt 3, Prim Int)
              , Arrow (Prim Int, Arrow (Prim Int, Prim Int)) )
          , Prim Int )
      ]
    in
    anf e |> print_anfstatements
  in
  [%expect
    {|
    [(AnfLet ("x", [],
        (ALet ("#binop1", (CPlus ((ImmNum 1), (ImmNum 2))),
           (ALet ("#binop2", (CMultiply ((ImmId "#binop1"), (ImmNum 3))),
              (ACEexpr (CImmExpr (ImmId "#binop2")))))
           ))
        ))
      ]
 |}]
;;

let%expect_test _ =
  (*
     Input:
     let x y = (6 + 9) * (4 + y) / y

     Output:
     let x y =
     let #binop1 = 6 + 9 in
     let #binop2 = 4 + y in
     let #binop3 = #binop1 * #binop2 in
     let #binop4  = #binop3  / y in
     #binop4
  *)
  let _ =
    let e =
      [ LLet
          ( "x"
          , [ Arg ("y", Prim Int) ]
          , LBinop
              ( Div
              , LBinop
                  ( Mul
                  , LBinop
                      ( Add
                      , LConst (CInt 6, Prim Int)
                      , LConst (CInt 9, Prim Int)
                      , Arrow (Prim Int, Arrow (Prim Int, Prim Int)) )
                  , LBinop
                      ( Add
                      , LConst (CInt 4, Prim Int)
                      , LVar ("y", Prim Int)
                      , Arrow (Prim Int, Arrow (Prim Int, Prim Int)) )
                  , Arrow (Prim Int, Arrow (Prim Int, Prim Int)) )
              , LVar ("y", Prim Int)
              , Arrow (Prim Int, Arrow (Prim Int, Prim Int)) )
          , Arrow (Prim Int, Prim Int) )
      ]
    in
    anf e |> print_anfstatements
  in
  [%expect
    {|
    [(AnfLet ("x", ["y"],
        (ALet ("#binop1", (CPlus ((ImmNum 6), (ImmNum 9))),
           (ALet ("#binop2", (CPlus ((ImmNum 4), (ImmId "y"))),
              (ALet ("#binop3",
                 (CMultiply ((ImmId "#binop1"), (ImmId "#binop2"))),
                 (ALet ("#binop4", (CDivide ((ImmId "#binop3"), (ImmId "y"))),
                    (ACEexpr (CImmExpr (ImmId "#binop4")))))
                 ))
              ))
           ))
        ))
      ]
 |}]
;;

let%expect_test _ =
  (*
     Input:
     let test f x = f x 1

     Output:
     let test f x =
     let #app1 = f x in
     let #app2 = #app1 1 in
     #app2
  *)
  let _ =
    let e =
      [ LLet
          ( "test"
          , [ Arg ("f", Arrow (Tyvar 1, Arrow (Prim Int, Tyvar 3))); Arg ("x", Tyvar 1) ]
          , LApp
              ( LApp
                  ( LVar ("f", Arrow (Tyvar 1, Arrow (Prim Int, Tyvar 3)))
                  , LVar ("x", Tyvar 1)
                  , Arrow (Tyvar 1, Arrow (Prim Int, Tyvar 3)) )
              , LConst (CInt 1, Prim Int)
              , Tyvar 3 )
          , Arrow (Arrow (Tyvar 1, Arrow (Prim Int, Tyvar 3)), Arrow (Tyvar 1, Tyvar 3))
          )
      ]
    in
    anf e |> print_anfstatements
  in
  [%expect
    {|
    [(AnfLet ("test", ["f"; "x"],
        (ALet ("#app1", (CApp ((ImmId "f"), (ImmId "x"))),
           (ALet ("#app2", (CApp ((ImmId "#app1"), (ImmNum 1))),
              (ACEexpr (CImmExpr (ImmId "#app2")))))
           ))
        ))
      ]
 |}]
;;

let%expect_test _ =
  (*
     Input:
     let test f x y = if f 1 2 then x + 1 + 3 else y + 4 + 5

     Output:
     let test f x y =
     let #app1 = f 1 in
     let #app2 = #app1 2 in
     if #app2
     then
     ( let #binop5 = x + 1 in
     let  #binop6 =  #binop5 + 3 in #binop6)
     else
     ( let #binop3 = y + 4 in
     let  #binop4 =  #binop3 + 5 in #binop4)
  *)
  let _ =
    let e =
      [ LLet
          ( "test"
          , [ Arg ("f", Arrow (Prim Int, Arrow (Prim Int, Prim Bool)))
            ; Arg ("x", Prim Int)
            ; Arg ("y", Prim Int)
            ]
          , LIfThenElse
              ( LApp
                  ( LApp
                      ( LVar ("f", Arrow (Prim Int, Arrow (Prim Int, Prim Bool)))
                      , LConst (CInt 1, Prim Int)
                      , Arrow (Prim Int, Arrow (Prim Int, Prim Bool)) )
                  , LConst (CInt 2, Prim Int)
                  , Prim Bool )
              , LBinop
                  ( Add
                  , LBinop
                      ( Add
                      , LVar ("x", Prim Int)
                      , LConst (CInt 1, Prim Int)
                      , Arrow (Prim Int, Arrow (Prim Int, Prim Int)) )
                  , LConst (CInt 3, Prim Int)
                  , Arrow (Prim Int, Arrow (Prim Int, Prim Int)) )
              , LBinop
                  ( Add
                  , LBinop
                      ( Add
                      , LVar ("y", Prim Int)
                      , LConst (CInt 4, Prim Int)
                      , Arrow (Prim Int, Arrow (Prim Int, Prim Int)) )
                  , LConst (CInt 5, Prim Int)
                  , Arrow (Prim Int, Arrow (Prim Int, Prim Int)) )
              , Prim Int )
          , Arrow
              ( Arrow (Prim Int, Arrow (Prim Int, Prim Bool))
              , Arrow (Prim Int, Arrow (Prim Int, Prim Int)) ) )
      ]
    in
    anf e |> print_anfstatements
  in
  [%expect
    {|
    [(AnfLet ("test", ["f"; "x"; "y"],
        (ALet ("#app1", (CApp ((ImmId "f"), (ImmNum 1))),
           (ALet ("#app2", (CApp ((ImmId "#app1"), (ImmNum 2))),
              (AIfThenElse ((ACEexpr (CImmExpr (ImmId "#app2"))),
                 (ALet ("#binop5", (CPlus ((ImmId "x"), (ImmNum 1))),
                    (ALet ("#binop6", (CPlus ((ImmId "#binop5"), (ImmNum 3))),
                       (ACEexpr (CImmExpr (ImmId "#binop6")))))
                    )),
                 (ALet ("#binop3", (CPlus ((ImmId "y"), (ImmNum 4))),
                    (ALet ("#binop4", (CPlus ((ImmId "#binop3"), (ImmNum 5))),
                       (ACEexpr (CImmExpr (ImmId "#binop4")))))
                    ))
                 ))
              ))
           ))
        ))
      ]
 |}]
;;

let%expect_test _ =
  (*
     Input:
     let rec fact n acc =
     if n < 1 then acc else fact (n - 1) (n * acc)

     let fac_tailrec n = fact n 1

     Output:
     let rec fact n acc =
     let #binop1 = n < 1 in
     if #binop1
     then
     acc
     else
     let #binop2 = n - 1 in
     let #app3 = fact #binop2 in
     let #binop4 = acc * n in
     let #app5 = #app3 #binop4 in
     #app5

     let fac_tailrec n =
     let #app1 = fact n in
     let #app2 = #app1 1 in
     #app2
  *)
  let _ =
    let e =
      [ LLetRec
          ( "fact"
          , [ Arg ("n", Prim Int); Arg ("acc", Prim Int) ]
          , LIfThenElse
              ( LBinop
                  ( Lt
                  , LVar ("n", Prim Int)
                  , LConst (CInt 1, Prim Int)
                  , Arrow (Prim Int, Arrow (Prim Int, Prim Bool)) )
              , LVar ("acc", Prim Int)
              , LApp
                  ( LApp
                      ( LVar ("fact", Arrow (Prim Int, Arrow (Prim Int, Prim Int)))
                      , LBinop
                          ( Sub
                          , LVar ("n", Prim Int)
                          , LConst (CInt 1, Prim Int)
                          , Arrow (Prim Int, Arrow (Prim Int, Prim Int)) )
                      , Arrow (Prim Int, Arrow (Prim Int, Prim Int)) )
                  , LBinop
                      ( Mul
                      , LVar ("acc", Prim Int)
                      , LVar ("n", Prim Int)
                      , Arrow (Prim Int, Arrow (Prim Int, Prim Int)) )
                  , Prim Int )
              , Prim Int )
          , Arrow (Prim Int, Arrow (Prim Int, Prim Int)) )
      ; LLet
          ( "fac_tailrec"
          , [ Arg ("n", Prim Int) ]
          , LApp
              ( LApp
                  ( LVar ("fact", Arrow (Prim Int, Arrow (Prim Int, Prim Int)))
                  , LVar ("n", Prim Int)
                  , Arrow (Prim Int, Arrow (Prim Int, Prim Int)) )
              , LConst (CInt 1, Prim Int)
              , Prim Int )
          , Arrow (Prim Int, Prim Int) )
      ]
    in
    anf e |> print_anfstatements
  in
  [%expect
    {|
    [(AnfLetRec ("fact", ["n"; "acc"],
        (ALet ("#binop1", (CLt ((ImmId "n"), (ImmNum 1))),
           (AIfThenElse ((ACEexpr (CImmExpr (ImmId "#binop1"))),
              (ACEexpr (CImmExpr (ImmId "acc"))),
              (ALet ("#binop2", (CMinus ((ImmId "n"), (ImmNum 1))),
                 (ALet ("#app3", (CApp ((ImmId "fact"), (ImmId "#binop2"))),
                    (ALet ("#binop4", (CMultiply ((ImmId "acc"), (ImmId "n"))),
                       (ALet ("#app5",
                          (CApp ((ImmId "#app3"), (ImmId "#binop4"))),
                          (ACEexpr (CImmExpr (ImmId "#app5")))))
                       ))
                    ))
                 ))
              ))
           ))
        ));
      (AnfLet ("fac_tailrec", ["n"],
         (ALet ("#app1", (CApp ((ImmId "fact"), (ImmId "n"))),
            (ALet ("#app2", (CApp ((ImmId "#app1"), (ImmNum 1))),
               (ACEexpr (CImmExpr (ImmId "#app2")))))
            ))
         ))
      ]
 |}]
;;
