(** Copyright 2023-2024, Rustam Shangareev and Danil Yevdokimov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

open ZRusML_lib.Parser
open ZRusML_lib.Pretty_printer
open ZRusML_lib.Lambda_lifting

let helper code =
  match parse prog code with
  | Error _ -> Printf.printf "PARSE ERROR!"
  | Result.Ok res -> pp_prog Format.std_formatter (lift_prog res)
;;

let%expect_test _ =
  let code =
    {|
  let a =
    let m = c + d in
    let k =
      (fun m l ->
        let x = l * 2 in
        let y = (fun m t -> m + t) m in
        y x)
        m
    in
    k (5 + m)
;;
  |}
  in
  helper code;
  [%expect
    {|
let 0lambda m t = (m + t);;
let 1lambda m l = 
    let x = (l * 2) in
    let y = 0lambda m in
    y x
;;
let a = 
    let m = (c + d) in
    let k = 1lambda m in
    k (5 + m)
;;
  |}]
;;

let%expect_test _ =
  let code = {|
    let f = (fun y x -> (x + y)) y;;
  |} in
  helper code;
  [%expect {|
let 0lambda y x = (x + y);;
let f = 0lambda y;;
  |}]
;;

let%expect_test _ =
  let code =
    {|
    let rec f x y = x + f y;;
    let a b c =
      let t = 15 in
      let rec f x y = f x + f y + 3 in
      f (f x) y
;;
  |}
  in
  helper code;
  [%expect
    {|
let rec 0lambda x y = (x + 0lambda y);;
let rec 1lambda x y = (1lambda x + (1lambda y + 3));;
let 2lambda b c = 
    let t = 15 in
    let rec f = 1lambda in
    f (f x) y
;;
let rec f = 0lambda;;
let a = 2lambda;;
  |}]
;;

let%expect_test _ =
  let code =
    {|
    let fac n =
      let rec fack n k =
        if n <= 1 then 1
        else fack (n - 1) (fun m -> k (m * n))
      in
      fack n (fun x -> x)
;;
|}
  in
  helper code;
  [%expect
    {|
let 0lambda m = k (m * n);;
let rec 1lambda n k = if (n <= 1) then 1 else 1lambda (n - 1) 0lambda;;
let 2lambda x = x;;
let 3lambda n = 
    let rec fack = 1lambda in
    fack n 2lambda
;;
let fac = 3lambda;;
|}]
;;
