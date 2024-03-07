(** Copyright 2023-2024, Ilya Pankratov, Maxim Drumov *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open Jaml_lib
open Jaml_lib.Parser

let run_closure_test test_case =
  let open Pprinttypedtree in
  let fmt = Format.std_formatter in
  match parse test_case with
  | Error err -> pp_error fmt err
  | Ok commands ->
    (match Inferencer.infer Inferencer.Enable commands with
     | Error err -> Inferencer.pp_error fmt err
     | Ok typed_commands ->
       Format.printf "%a" pp_statements_without_types (Closure.closure typed_commands))
;;

let%expect_test _ =
  let _ =
    let test = {|
     let sum x =
     let new_sum y = x + y in
     new_sum 5
    |} in
    run_closure_test test
  in
  [%expect
    {|
    let sum = fun x ->
        let new_sum = fun x -> fun y -> (x + y) in ((new_sum x) 5)
 |}]
;;

let%expect_test _ =
  let _ =
    let test = {|
     let x y =
     let z a = a (y + 1) in
     z (fun x -> x)
  |} in
    run_closure_test test
  in
  [%expect
    {|
    let x = fun y ->
        let #closure_fun1 = fun x -> x in
        let z = fun y -> fun a -> (a (y + 1)) in ((z y) #closure_fun1)
 |}]
;;

let%expect_test _ =
  let _ =
    let test =
      {|
     let fac n =
     let rec fack n k =
     if n <= 1 then k 1
     else fack (n-1) ((fun k n m -> k (m * n)) k n) 
     in
     fack n (fun x -> x)
  |}
    in
    run_closure_test test
  in
  [%expect
    {|
    let fac = fun n ->
        let #closure_fun1 = fun k -> fun n -> fun m -> (k (m * n)) in
        let #closure_fun2 = fun x -> x in
        let rec fack = fun n -> fun k ->
        if (n <= 1) then (k 1) else ((fack (n - 1)) ((#closure_fun1 k) n)) in ((fack n) #closure_fun2)
 |}]
;;

let%expect_test _ =
  let _ =
    let test =
      {|
      let fibo n =
        let rec fibo_cps n acc =
        if n < 3
        then acc 1
        else fibo_cps (n - 1) (fun x -> fibo_cps (n - 2) (fun y -> acc (x + y)))
        in
        fibo_cps n (fun x -> x)
  |}
    in
    run_closure_test test
  in
  [%expect
    {|
    let fibo = fun n ->
        let #closure_fun1 = fun x -> fun acc -> fun y -> (acc (x + y)) in
        let #closure_fun2 = fun n -> fun fibo_cps -> fun acc -> fun x -> ((fibo_cps (n - 2)) ((#closure_fun1 x) acc)) in
        let #closure_fun3 = fun x -> x in
        let rec fibo_cps = fun n -> fun acc ->
        if (n < 3) then (acc 1) else ((fibo_cps (n - 1)) (((#closure_fun2 n) fibo_cps) acc)) in ((fibo_cps n) #closure_fun3)
 |}]
;;

let%expect_test _ =
  let _ =
    let test =
      {|
     let sum x =
     let new_x = x + 1 in
     let new_sum = new_x + 1 in
     new_sum
  |}
    in
    run_closure_test test
  in
  [%expect
    {|
    let sum = fun x ->
        let new_x = (x + 1) in
        let new_sum = (new_x + 1) in new_sum
 |}]
;;

let%expect_test _ =
  let _ =
    let test =
      {|
      let x c (a,b) =
        let sum (c, d) = (a + b, c + d) in  
        sum (c, 1)
        |}
    in
    run_closure_test test
  in
  [%expect
    {|
    let x = fun c -> fun (a, b) ->
        let sum = fun b -> fun a -> fun (c, d) -> ((a + b), (c + d)) in (((sum b) a) (c, 1))
 |}]
;;

let%expect_test _ =
  let _ =
    let test =
      {|
      let x (a,b,c,d) = 
        let f a b = (a + b, if c < 1 then c else d) in
        f a b
        |}
    in
    run_closure_test test
  in
  [%expect
    {|
    let x = fun (a, b, c, d) ->
        let f = fun d -> fun c -> fun a -> fun b -> ((a + b),
        if (c < 1) then c else d) in ((((f d) c) a) b)
 |}]
;;

let%expect_test _ =
  let _ =
    let test =
      {|
      let f x = 
        let sum a = (fun (k, j) a -> (k + j) * a) x a in
        let fst (a, _) = a in
        let scd (_, b) = b in
        (sum (fst x), sum (scd x))
        |}
    in
    run_closure_test test
  in
  [%expect
    {|
    let f = fun x ->
        let #closure_fun1 = fun (k, j) -> fun a -> ((k + j) * a) in
        let sum = fun x -> fun a -> ((#closure_fun1 x) a) in
        let fst = fun (a, _) -> a in
        let scd = fun (_, b) -> b in (((sum x) (fst x)), ((sum x) (scd x)))
 |}]
;;
