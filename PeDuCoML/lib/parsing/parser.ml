(** Copyright 2023-2024, Danila Pechenev, Ilya Dudnikov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Lexer
open Lexing
open Result
open Ast

let print_position outx lexbuf =
  let pos = lexbuf.lex_curr_p in
  Format.fprintf
    outx
    "%s:%d:%d"
    pos.pos_fname
    pos.pos_lnum
    (pos.pos_cnum - pos.pos_bol + 1)
;;

let parse code : (Ast.declaration list, string) result =
  let lexbuf = Lexing.from_string code in
  try Menhirparser.parse token lexbuf |> ok with
  | Menhirparser.Error ->
    let error_message = Format.asprintf "%a: syntax error" print_position lexbuf in
    error error_message
;;

(* 1 *)
let%test _ =
  parse
    "let rec factorial n acc = if n <= 1 then acc else factorial (n - 1) (acc * n)\n\
     let main = factorial 5 1 "
  = ok
      [ DRecursiveDeclaration
          ( "factorial"
          , [ PIdentifier "n"; PIdentifier "acc" ]
          , EIf
              ( EBinaryOperation (LTE, EIdentifier "n", ELiteral (LInt 1))
              , EIdentifier "acc"
              , EApplication
                  ( EApplication
                      ( EIdentifier "factorial"
                      , EBinaryOperation (Sub, EIdentifier "n", ELiteral (LInt 1)) )
                  , EBinaryOperation (Mul, EIdentifier "acc", EIdentifier "n") ) ) )
      ; DDeclaration
          ( "main"
          , []
          , EApplication
              ( EApplication (EIdentifier "factorial", ELiteral (LInt 5))
              , ELiteral (LInt 1) ) )
      ]
;;

(* 2 *)
let%test _ =
  parse " let main = 1 :: 2 :: [] "
  = ok
      [ DDeclaration
          ( "main"
          , []
          , EConstructList
              (ELiteral (LInt 1), EConstructList (ELiteral (LInt 2), EList [])) )
      ]
;;

(* 3 *)
let%test _ =
  parse " let main = true :: (false) :: [false] "
  = ok
      [ DDeclaration
          ( "main"
          , []
          , EConstructList
              ( ELiteral (LBool true)
              , EConstructList (ELiteral (LBool false), EList [ ELiteral (LBool false) ])
              ) )
      ]
;;

(* 4 *)
let%test _ =
  parse " let main = (10 + 20) :: [30; 4 * 10] "
  = ok
      [ DDeclaration
          ( "main"
          , []
          , EConstructList
              ( EBinaryOperation (Add, ELiteral (LInt 10), ELiteral (LInt 20))
              , EList
                  [ ELiteral (LInt 30)
                  ; EBinaryOperation (Mul, ELiteral (LInt 4), ELiteral (LInt 10))
                  ] ) )
      ]
;;

(* 5 *)
let%test _ =
  parse " let main = (fun x -> 'a') :: [fun _ -> 'b'] "
  = ok
      [ DDeclaration
          ( "main"
          , []
          , EConstructList
              ( EFun (PIdentifier "x", [], ELiteral (LChar 'a'))
              , EList [ EFun (PWildcard, [], ELiteral (LChar 'b')) ] ) )
      ]
;;

(* 6 *)
let%test _ =
  parse " let main = 0 :: (0) :: ((0)) :: (((0))) :: [] "
  = ok
      [ DDeclaration
          ( "main"
          , []
          , EConstructList
              ( ELiteral (LInt 0)
              , EConstructList
                  ( ELiteral (LInt 0)
                  , EConstructList
                      (ELiteral (LInt 0), EConstructList (ELiteral (LInt 0), EList [])) )
              ) )
      ]
;;

(* 7 *)
let%test _ =
  parse " let main = [\"apple\";\n\"orange\";\n\"banana\";\n\"pear\"] "
  = ok
      [ DDeclaration
          ( "main"
          , []
          , EList
              [ ELiteral (LString "apple")
              ; ELiteral (LString "orange")
              ; ELiteral (LString "banana")
              ; ELiteral (LString "pear")
              ] )
      ]
;;

(* 8 *)
let%test _ =
  parse " let main = [ 'h' ; 'e' ; 'l' ; 'l' ; 'o' ] "
  = ok
      [ DDeclaration
          ( "main"
          , []
          , EList
              [ ELiteral (LChar 'h')
              ; ELiteral (LChar 'e')
              ; ELiteral (LChar 'l')
              ; ELiteral (LChar 'l')
              ; ELiteral (LChar 'o')
              ] )
      ]
;;

(* 9 *)
let%test _ =
  parse " let main = [1] " = ok [ DDeclaration ("main", [], EList [ ELiteral (LInt 1) ]) ]
;;

(* 10 *)
let%test _ = parse " let main = [] " = ok [ DDeclaration ("main", [], EList []) ]

(* 11 *)
let%test _ =
  parse
    " let main = [let x = 5 and y = 7 in x + y; (fun t -> t - 1) 10; if (5 >= 1) then 1 \
     else 0] "
  = ok
      [ DDeclaration
          ( "main"
          , []
          , EList
              [ ELetIn
                  ( DDeclaration ("x", [], ELiteral (LInt 5))
                  , [ DDeclaration ("y", [], ELiteral (LInt 7)) ]
                  , EBinaryOperation (Add, EIdentifier "x", EIdentifier "y") )
              ; EApplication
                  ( EFun
                      ( PIdentifier "t"
                      , []
                      , EBinaryOperation (Sub, EIdentifier "t", ELiteral (LInt 1)) )
                  , ELiteral (LInt 10) )
              ; EIf
                  ( EBinaryOperation (GTE, ELiteral (LInt 5), ELiteral (LInt 1))
                  , ELiteral (LInt 1)
                  , ELiteral (LInt 0) )
              ] )
      ]
;;

(* 12 *)
let%test _ =
  parse
    " let main = (if x > 0 then 1 else (if x = 0 then 0 else -1)) :: [0 ; (if y > 0 then \
     1 else (if y = 0 then 0 else -1)) ; 0] "
  = ok
      [ DDeclaration
          ( "main"
          , []
          , EConstructList
              ( EIf
                  ( EBinaryOperation (GT, EIdentifier "x", ELiteral (LInt 0))
                  , ELiteral (LInt 1)
                  , EIf
                      ( EBinaryOperation (Eq, EIdentifier "x", ELiteral (LInt 0))
                      , ELiteral (LInt 0)
                      , EUnaryOperation (Minus, ELiteral (LInt 1)) ) )
              , EList
                  [ ELiteral (LInt 0)
                  ; EIf
                      ( EBinaryOperation (GT, EIdentifier "y", ELiteral (LInt 0))
                      , ELiteral (LInt 1)
                      , EIf
                          ( EBinaryOperation (Eq, EIdentifier "y", ELiteral (LInt 0))
                          , ELiteral (LInt 0)
                          , EUnaryOperation (Minus, ELiteral (LInt 1)) ) )
                  ; ELiteral (LInt 0)
                  ] ) )
      ]
;;

(* 13 *)
let%test _ =
  parse " let main = fun x y z -> x + y * z "
  = ok
      [ DDeclaration
          ( "main"
          , []
          , EFun
              ( PIdentifier "x"
              , [ PIdentifier "y"; PIdentifier "z" ]
              , EBinaryOperation
                  ( Add
                  , EIdentifier "x"
                  , EBinaryOperation (Mul, EIdentifier "y", EIdentifier "z") ) ) )
      ]
;;

(* 14 *)
let%test _ =
  parse " let main = fun _ -> 42 "
  = ok [ DDeclaration ("main", [], EFun (PWildcard, [], ELiteral (LInt 42))) ]
;;

(* 15 *)
let%test _ =
  parse " let main = fun _ -> fun _ -> \"Hello\" "
  = ok
      [ DDeclaration
          ( "main"
          , []
          , EFun (PWildcard, [], EFun (PWildcard, [], ELiteral (LString "Hello"))) )
      ]
;;

(* 16 *)
let%test _ =
  parse " let main = fun x y -> if x < 0 then [x;y] else [0;y] "
  = ok
      [ DDeclaration
          ( "main"
          , []
          , EFun
              ( PIdentifier "x"
              , [ PIdentifier "y" ]
              , EIf
                  ( EBinaryOperation (LT, EIdentifier "x", ELiteral (LInt 0))
                  , EList [ EIdentifier "x"; EIdentifier "y" ]
                  , EList [ ELiteral (LInt 0); EIdentifier "y" ] ) ) )
      ]
;;

(* 17 *)
let%test _ =
  parse
    " let rec matrix_mult_number matrix number =\n\
    \     let rec line_mult_number line =\n\
    \       match line with\n\
    \         | head :: tail -> (head * number) :: line_mult_number tail\n\
    \         | _ -> []\n\
    \     in\n\
    \     match matrix with\n\
    \       | head :: tail -> line_mult_number head :: matrix_mult_number tail number\n\
    \       | _ -> []"
  = ok
      [ DRecursiveDeclaration
          ( "matrix_mult_number"
          , [ PIdentifier "matrix"; PIdentifier "number" ]
          , ELetIn
              ( DRecursiveDeclaration
                  ( "line_mult_number"
                  , [ PIdentifier "line" ]
                  , EMatchWith
                      ( EIdentifier "line"
                      , ( PConstructList (PIdentifier "head", PIdentifier "tail")
                        , EConstructList
                            ( EBinaryOperation
                                (Mul, EIdentifier "head", EIdentifier "number")
                            , EApplication
                                (EIdentifier "line_mult_number", EIdentifier "tail") ) )
                      , [ PWildcard, EList [] ] ) )
              , []
              , EMatchWith
                  ( EIdentifier "matrix"
                  , ( PConstructList (PIdentifier "head", PIdentifier "tail")
                    , EConstructList
                        ( EApplication (EIdentifier "line_mult_number", EIdentifier "head")
                        , EApplication
                            ( EApplication
                                (EIdentifier "matrix_mult_number", EIdentifier "tail")
                            , EIdentifier "number" ) ) )
                  , [ PWildcard, EList [] ] ) ) )
      ]
;;

(* 18 *)
let%test _ =
  parse " let main = \"Danya\", \"Ilya\" "
  = ok
      [ DDeclaration
          ("main", [], ETuple (ELiteral (LString "Danya"), ELiteral (LString "Ilya"), []))
      ]
;;

(* 19 *)
let%test _ =
  parse " let main = ( 123\t, \"aaa\"\t, 'b'\n, true\t ) "
  = ok
      [ DDeclaration
          ( "main"
          , []
          , ETuple
              ( ELiteral (LInt 123)
              , ELiteral (LString "aaa")
              , [ ELiteral (LChar 'b'); ELiteral (LBool true) ] ) )
      ]
;;

(* 20 *)
let%test _ =
  parse " let main = (fun _ -> 1, fun _ -> 2) "
  = ok
      [ DDeclaration
          ( "main"
          , []
          , EFun
              ( PWildcard
              , []
              , ETuple (ELiteral (LInt 1), EFun (PWildcard, [], ELiteral (LInt 2)), []) )
          )
      ]
;;

(* 21 *)
let%test _ =
  parse " let main = [fun _ -> 1; fun _ -> 2] "
  = ok
      [ DDeclaration
          ( "main"
          , []
          , EList
              [ EFun (PWildcard, [], ELiteral (LInt 1))
              ; EFun (PWildcard, [], ELiteral (LInt 2))
              ] )
      ]
;;

(* 22 *)
let%test _ =
  parse " let main = f (g 5, h (0)) (let x = 17 and y = 6 and z = 3 in x * y / z) "
  = ok
      [ DDeclaration
          ( "main"
          , []
          , EApplication
              ( EApplication
                  ( EIdentifier "f"
                  , ETuple
                      ( EApplication (EIdentifier "g", ELiteral (LInt 5))
                      , EApplication (EIdentifier "h", ELiteral (LInt 0))
                      , [] ) )
              , ELetIn
                  ( DDeclaration ("x", [], ELiteral (LInt 17))
                  , [ DDeclaration ("y", [], ELiteral (LInt 6))
                    ; DDeclaration ("z", [], ELiteral (LInt 3))
                    ]
                  , EBinaryOperation
                      ( Div
                      , EBinaryOperation (Mul, EIdentifier "x", EIdentifier "y")
                      , EIdentifier "z" ) ) ) )
      ]
;;

(* 23 *)
let%test _ =
  parse
    " let main = func (if x > 0 && y < 0 then x * y else 0) (let f t = t * t * t in (f \
     x) * (f x)) ([]) "
  = ok
      [ DDeclaration
          ( "main"
          , []
          , EApplication
              ( EApplication
                  ( EApplication
                      ( EIdentifier "func"
                      , EIf
                          ( EBinaryOperation
                              ( AND
                              , EBinaryOperation (GT, EIdentifier "x", ELiteral (LInt 0))
                              , EBinaryOperation (LT, EIdentifier "y", ELiteral (LInt 0))
                              )
                          , EBinaryOperation (Mul, EIdentifier "x", EIdentifier "y")
                          , ELiteral (LInt 0) ) )
                  , ELetIn
                      ( DDeclaration
                          ( "f"
                          , [ PIdentifier "t" ]
                          , EBinaryOperation
                              ( Mul
                              , EBinaryOperation (Mul, EIdentifier "t", EIdentifier "t")
                              , EIdentifier "t" ) )
                      , []
                      , EBinaryOperation
                          ( Mul
                          , EApplication (EIdentifier "f", EIdentifier "x")
                          , EApplication (EIdentifier "f", EIdentifier "x") ) ) )
              , EList [] ) )
      ]
;;

(* 24 *)
let%test _ =
  parse
    " let main = if x * y / (z * z) > 15 || (f t) <= 0 || (fun x -> x * x) r >= 100 then \
     x - y - z * r else -1 "
  = ok
      [ DDeclaration
          ( "main"
          , []
          , EIf
              ( EBinaryOperation
                  ( OR
                  , EBinaryOperation
                      ( OR
                      , EBinaryOperation
                          ( GT
                          , EBinaryOperation
                              ( Div
                              , EBinaryOperation (Mul, EIdentifier "x", EIdentifier "y")
                              , EBinaryOperation (Mul, EIdentifier "z", EIdentifier "z")
                              )
                          , ELiteral (LInt 15) )
                      , EBinaryOperation
                          ( LTE
                          , EApplication (EIdentifier "f", EIdentifier "t")
                          , ELiteral (LInt 0) ) )
                  , EBinaryOperation
                      ( GTE
                      , EApplication
                          ( EFun
                              ( PIdentifier "x"
                              , []
                              , EBinaryOperation (Mul, EIdentifier "x", EIdentifier "x")
                              )
                          , EIdentifier "r" )
                      , ELiteral (LInt 100) ) )
              , EBinaryOperation
                  ( Sub
                  , EBinaryOperation (Sub, EIdentifier "x", EIdentifier "y")
                  , EBinaryOperation (Mul, EIdentifier "z", EIdentifier "r") )
              , EUnaryOperation (Minus, ELiteral (LInt 1)) ) )
      ]
;;

(* 25 *)
let%test _ =
  parse " let main = if not(x = 5) && y = 5 then f x else f y "
  = ok
      [ DDeclaration
          ( "main"
          , []
          , EIf
              ( EBinaryOperation
                  ( AND
                  , EUnaryOperation
                      (Not, EBinaryOperation (Eq, EIdentifier "x", ELiteral (LInt 5)))
                  , EBinaryOperation (Eq, EIdentifier "y", ELiteral (LInt 5)) )
              , EApplication (EIdentifier "f", EIdentifier "x")
              , EApplication (EIdentifier "f", EIdentifier "y") ) )
      ]
;;

(* 26 *)
let%test _ =
  parse
    " let phi n = let rec helper last1 last2 n = if n > 0 then helper last2 (last1 + \
     last2) (n - 1) else last2 in\n\
    \  helper 1 1 (n - 2) "
  = ok
      [ DDeclaration
          ( "phi"
          , [ PIdentifier "n" ]
          , ELetIn
              ( DRecursiveDeclaration
                  ( "helper"
                  , [ PIdentifier "last1"; PIdentifier "last2"; PIdentifier "n" ]
                  , EIf
                      ( EBinaryOperation (GT, EIdentifier "n", ELiteral (LInt 0))
                      , EApplication
                          ( EApplication
                              ( EApplication (EIdentifier "helper", EIdentifier "last2")
                              , EBinaryOperation
                                  (Add, EIdentifier "last1", EIdentifier "last2") )
                          , EBinaryOperation (Sub, EIdentifier "n", ELiteral (LInt 1)) )
                      , EIdentifier "last2" ) )
              , []
              , EApplication
                  ( EApplication
                      ( EApplication (EIdentifier "helper", ELiteral (LInt 1))
                      , ELiteral (LInt 1) )
                  , EBinaryOperation (Sub, EIdentifier "n", ELiteral (LInt 2)) ) ) )
      ]
;;

(* 27 *)
let%test _ =
  parse " let main = let sq x = x * x in sq x + sq y + sq z "
  = ok
      [ DDeclaration
          ( "main"
          , []
          , ELetIn
              ( DDeclaration
                  ( "sq"
                  , [ PIdentifier "x" ]
                  , EBinaryOperation (Mul, EIdentifier "x", EIdentifier "x") )
              , []
              , EBinaryOperation
                  ( Add
                  , EBinaryOperation
                      ( Add
                      , EApplication (EIdentifier "sq", EIdentifier "x")
                      , EApplication (EIdentifier "sq", EIdentifier "y") )
                  , EApplication (EIdentifier "sq", EIdentifier "z") ) ) )
      ]
;;

(* 28 *)
let%test _ =
  parse " let mult x y z = x * y * z \n  let main = mult 5 6 7 "
  = ok
      [ DDeclaration
          ( "mult"
          , [ PIdentifier "x"; PIdentifier "y"; PIdentifier "z" ]
          , EBinaryOperation
              ( Mul
              , EBinaryOperation (Mul, EIdentifier "x", EIdentifier "y")
              , EIdentifier "z" ) )
      ; DDeclaration
          ( "main"
          , []
          , EApplication
              ( EApplication
                  (EApplication (EIdentifier "mult", ELiteral (LInt 5)), ELiteral (LInt 6))
              , ELiteral (LInt 7) ) )
      ]
;;

(* 29 *)
let%test _ =
  parse
    " let f x y z = match x, y, z with\n\
    \  | true, true, false -> true\n\
    \  | true, false, true -> true\n\
    \  | false, true, true -> true\n\
    \  | _ -> false"
  = ok
      [ DDeclaration
          ( "f"
          , [ PIdentifier "x"; PIdentifier "y"; PIdentifier "z" ]
          , EMatchWith
              ( ETuple (EIdentifier "x", EIdentifier "y", [ EIdentifier "z" ])
              , ( PTuple
                    ( PLiteral (LBool true)
                    , PLiteral (LBool true)
                    , [ PLiteral (LBool false) ] )
                , ELiteral (LBool true) )
              , [ ( PTuple
                      ( PLiteral (LBool true)
                      , PLiteral (LBool false)
                      , [ PLiteral (LBool true) ] )
                  , ELiteral (LBool true) )
                ; ( PTuple
                      ( PLiteral (LBool false)
                      , PLiteral (LBool true)
                      , [ PLiteral (LBool true) ] )
                  , ELiteral (LBool true) )
                ; PWildcard, ELiteral (LBool false)
                ] ) )
      ]
;;

(* 30 *)
let%test _ =
  parse "let f 2 = 1"
  = ok [ DDeclaration ("f", [ PLiteral (LInt 2) ], ELiteral (LInt 1)) ]
;;

(* 31 *)
let%test _ =
  parse "let f _ = 1" = ok [ DDeclaration ("f", [ PWildcard ], ELiteral (LInt 1)) ]
;;

(* 32 *)
let%test _ =
  parse "let f (x, \"abacaba\") = 1"
  = ok
      [ DDeclaration
          ( "f"
          , [ PTuple (PIdentifier "x", PLiteral (LString "abacaba"), []) ]
          , ELiteral (LInt 1) )
      ]
;;

(* 33 *)
let%test _ =
  parse "let f [(x, y); ((_), _)] = 1"
  = ok
      [ DDeclaration
          ( "f"
          , [ PList
                [ PTuple (PIdentifier "x", PIdentifier "y", [])
                ; PTuple (PWildcard, PWildcard, [])
                ]
            ]
          , ELiteral (LInt 1) )
      ]
;;

(* 34 *)
let%test _ =
  parse "let f ((true, '\n') :: [x, y]) = 1"
  = ok
      [ DDeclaration
          ( "f"
          , [ PConstructList
                ( PTuple (PLiteral (LBool true), PLiteral (LChar '\n'), [])
                , PList [ PTuple (PIdentifier "x", PIdentifier "y", []) ] )
            ]
          , ELiteral (LInt 1) )
      ]
;;

(* 35 *)
let%test _ =
  parse "let f = fun x -> x"
  = ok [ DDeclaration ("f", [], EFun (PIdentifier "x", [], EIdentifier "x")) ]
;;

(* 36 *)
let%test _ =
  parse "let f = fun _ -> 2"
  = ok [ DDeclaration ("f", [], EFun (PWildcard, [], ELiteral (LInt 2))) ]
;;

(* 37 *)
let%test _ =
  parse "let f = fun x, (true, \"42\") -> true"
  = ok
      [ DDeclaration
          ( "f"
          , []
          , EFun
              ( PTuple
                  ( PIdentifier "x"
                  , PTuple (PLiteral (LBool true), PLiteral (LString "42"), [])
                  , [] )
              , []
              , ELiteral (LBool true) ) )
      ]
;;

(* 38 *)
let%test _ =
  parse "let f = fun [1, x, true; _, _, y] -> true"
  = ok
      [ DDeclaration
          ( "f"
          , []
          , EFun
              ( PList
                  [ PTuple (PLiteral (LInt 1), PIdentifier "x", [ PLiteral (LBool true) ])
                  ; PTuple (PWildcard, PWildcard, [ PIdentifier "y" ])
                  ]
              , []
              , ELiteral (LBool true) ) )
      ]
;;

(* 39 *)
let%test _ =
  parse "let f = fun ([x] :: [y; [(([])), ([], [])]]) -> true"
  = ok
      [ DDeclaration
          ( "f"
          , []
          , EFun
              ( PConstructList
                  ( PList [ PIdentifier "x" ]
                  , PList
                      [ PIdentifier "y"
                      ; PList [ PTuple (PList [], PTuple (PList [], PList [], []), []) ]
                      ] )
              , []
              , ELiteral (LBool true) ) )
      ]
;;

(* 40 *)
let%test _ =
  parse " let f x y z = match x, y, z with\n  | true, _, x -> true\n  | _ -> false"
  = ok
      [ DDeclaration
          ( "f"
          , [ PIdentifier "x"; PIdentifier "y"; PIdentifier "z" ]
          , EMatchWith
              ( ETuple (EIdentifier "x", EIdentifier "y", [ EIdentifier "z" ])
              , ( PTuple (PLiteral (LBool true), PWildcard, [ PIdentifier "x" ])
                , ELiteral (LBool true) )
              , [ PWildcard, ELiteral (LBool false) ] ) )
      ]
;;

(* 41 *)
let%test _ =
  parse
    " let f x y z = match x, y, z with\n\
    \  | [a; b], c :: [d; e], _ -> true\n\
    \  | _ -> false"
  = ok
      [ DDeclaration
          ( "f"
          , [ PIdentifier "x"; PIdentifier "y"; PIdentifier "z" ]
          , EMatchWith
              ( ETuple (EIdentifier "x", EIdentifier "y", [ EIdentifier "z" ])
              , ( PTuple
                    ( PList [ PIdentifier "a"; PIdentifier "b" ]
                    , PConstructList
                        (PIdentifier "c", PList [ PIdentifier "d"; PIdentifier "e" ])
                    , [ PWildcard ] )
                , ELiteral (LBool true) )
              , [ PWildcard, ELiteral (LBool false) ] ) )
      ]
;;

(* 42 *)
let%test _ =
  parse
    " let f x y z = match x, y, z with\n\
    \  | _, [a, 1; (_), _], (c, [d; _] :: _, []) -> true\n\
    \  | _ -> false"
  = ok
      [ DDeclaration
          ( "f"
          , [ PIdentifier "x"; PIdentifier "y"; PIdentifier "z" ]
          , EMatchWith
              ( ETuple (EIdentifier "x", EIdentifier "y", [ EIdentifier "z" ])
              , ( PTuple
                    ( PWildcard
                    , PList
                        [ PTuple (PIdentifier "a", PLiteral (LInt 1), [])
                        ; PTuple (PWildcard, PWildcard, [])
                        ]
                    , [ PTuple
                          ( PIdentifier "c"
                          , PConstructList
                              (PList [ PIdentifier "d"; PWildcard ], PWildcard)
                          , [ PList [] ] )
                      ] )
                , ELiteral (LBool true) )
              , [ PWildcard, ELiteral (LBool false) ] ) )
      ]
;;

(* 43 *)
let%test _ =
  parse "let f ___ = fun [1, x, true; _, _, y] -> true"
  = ok
      [ DDeclaration
          ( "f"
          , [ PWildcard ]
          , EFun
              ( PList
                  [ PTuple (PLiteral (LInt 1), PIdentifier "x", [ PLiteral (LBool true) ])
                  ; PTuple (PWildcard, PWildcard, [ PIdentifier "y" ])
                  ]
              , []
              , ELiteral (LBool true) ) )
      ]
;;

(* 44 *)
let%test _ =
  parse "let f (__) = fun [1, x, true; _, _, y] -> true"
  = ok
      [ DDeclaration
          ( "f"
          , [ PWildcard ]
          , EFun
              ( PList
                  [ PTuple (PLiteral (LInt 1), PIdentifier "x", [ PLiteral (LBool true) ])
                  ; PTuple (PWildcard, PWildcard, [ PIdentifier "y" ])
                  ]
              , []
              , ELiteral (LBool true) ) )
      ]
;;

(* 45 *)
let%test _ =
  parse "let f _x = fun [1, x, true; _, _, y] -> true"
  = ok
      [ DDeclaration
          ( "f"
          , [ PIdentifier "_x" ]
          , EFun
              ( PList
                  [ PTuple (PLiteral (LInt 1), PIdentifier "x", [ PLiteral (LBool true) ])
                  ; PTuple (PWildcard, PWildcard, [ PIdentifier "y" ])
                  ]
              , []
              , ELiteral (LBool true) ) )
      ]
;;
