(** Copyright 2023-2024, Rustam Shangareev and Danil Yevdokimov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

open ZRusML_lib.Ast
open ZRusML_lib.Parser

let test_parse ~label ~code ~expected =
  match parse prog code with
  | Error _ ->
    Printf.printf "[Parser test] %s -> PARSE ERROR" label;
    false
  | Result.Ok res when expected = res -> true
  | Result.Ok res ->
    let () =
      Printf.printf "[Parser test] %s failed.\nActual is:\n%s\n" label (show_prog res)
    in
    false
;;

let%test _ =
  test_parse
    ~label:"Fun and val test"
    ~code:
      {|

    let f = fun x -> fun y -> fun z -> (x + y * z) / 2 * 1337 + 9 / 13 - 14;;

    let f x y z = (x + y * z) / 2 * 1337 + 9 / 13 - 14;;

    |}
    ~expected:
      [ DLet
          ( false
          , PtVar "f"
          , EFun
              ( PtVar "x"
              , EFun
                  ( PtVar "y"
                  , EFun
                      ( PtVar "z"
                      , EBinOp
                          ( Add
                          , EBinOp
                              ( Div
                              , EBinOp (Add, EVar "x", EBinOp (Mul, EVar "y", EVar "z"))
                              , EBinOp (Mul, EConst (CInt 2), EConst (CInt 1337)) )
                          , EBinOp
                              ( Sub
                              , EBinOp (Div, EConst (CInt 9), EConst (CInt 13))
                              , EConst (CInt 14) ) ) ) ) ) )
      ; DLet
          ( false
          , PtVar "f"
          , EFun
              ( PtVar "x"
              , EFun
                  ( PtVar "y"
                  , EFun
                      ( PtVar "z"
                      , EBinOp
                          ( Add
                          , EBinOp
                              ( Div
                              , EBinOp (Add, EVar "x", EBinOp (Mul, EVar "y", EVar "z"))
                              , EBinOp (Mul, EConst (CInt 2), EConst (CInt 1337)) )
                          , EBinOp
                              ( Sub
                              , EBinOp (Div, EConst (CInt 9), EConst (CInt 13))
                              , EConst (CInt 14) ) ) ) ) ) )
      ]
;;

let%test _ =
  test_parse
    ~label:"fun test"
    ~code:
      {|

    let rec fact n = if n = 0 then 1 else n * fact (n-1);;

    let x = fact(3);;

    let _ = fact 5;;

    |}
    ~expected:
      [ DLet
          ( true
          , PtVar "fact"
          , EFun
              ( PtVar "n"
              , EIf
                  ( EBinOp (Eq, EVar "n", EConst (CInt 0))
                  , EConst (CInt 1)
                  , EBinOp
                      ( Mul
                      , EVar "n"
                      , EApp (EVar "fact", EBinOp (Sub, EVar "n", EConst (CInt 1))) ) ) )
          )
      ; DLet (false, PtVar "x", EApp (EVar "fact", EConst (CInt 3)))
      ; DLet (false, PtWild, EApp (EVar "fact", EConst (CInt 5)))
      ]
;;

let%test _ =
  test_parse
    ~label:"fib test"
    ~code:
      {|

      let rec fib n = if n = 0 || n = 1 then 1 else fib(n - 1) + fib(n - 2);;

      let tmp = fib 15 14 3;;

      let tmp =
        let check = fun f -> fun x -> if f x = x then f x else x
      in
        check;;

    |}
    ~expected:
      [ DLet
          ( true
          , PtVar "fib"
          , EFun
              ( PtVar "n"
              , EIf
                  ( EBinOp
                      ( Or
                      , EBinOp (Eq, EVar "n", EConst (CInt 0))
                      , EBinOp (Eq, EVar "n", EConst (CInt 1)) )
                  , EConst (CInt 1)
                  , EBinOp
                      ( Add
                      , EApp (EVar "fib", EBinOp (Sub, EVar "n", EConst (CInt 1)))
                      , EApp (EVar "fib", EBinOp (Sub, EVar "n", EConst (CInt 2))) ) ) )
          )
      ; DLet
          ( false
          , PtVar "tmp"
          , EApp
              ( EApp (EApp (EVar "fib", EConst (CInt 15)), EConst (CInt 14))
              , EConst (CInt 3) ) )
      ; DLet
          ( false
          , PtVar "tmp"
          , ELet
              ( [ ( false
                  , PtVar "check"
                  , EFun
                      ( PtVar "f"
                      , EFun
                          ( PtVar "x"
                          , EIf
                              ( EBinOp (Eq, EApp (EVar "f", EVar "x"), EVar "x")
                              , EApp (EVar "f", EVar "x")
                              , EVar "x" ) ) ) )
                ]
              , EVar "check" ) )
      ]
;;
