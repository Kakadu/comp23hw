(** Copyright 2023-2024, Rustam Shangareev and Danil Yevdokimov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

open ZRusML_lib.Closure_conversion
open ZRusML_lib.Parser
open ZRusML_lib.Pretty_printer

let helper code =
  match parse prog code with
  | Error _ -> Printf.printf "PARSE ERROR!"
  | Result.Ok res -> pp_prog Format.std_formatter (transform_decls res)
;;

let%expect_test "closure depth 0" =
  let code = {|
    let f x = x + y;;
|} in
  helper code;
  [%expect {|
  let f = (fun y x -> (x + y)) y;;
|}]
;;

let%expect_test "closure depth 1" =
  let code =
    {|
    let a c d =
      let m = c + d in
      let k l = l + m in
      k (5 + m)
    ;;
|}
  in
  helper code;
  [%expect
    {|
let a c d = 
    let m = (c + d) in
    let k = (fun m l -> (l + m)) m in
    k (5 + m)
;;
|}]
;;

let%expect_test "closure depth 2" =
  let code =
    {|
    let a c d =
      let m = c + d in
      let k l = 
        let x = l * 2 in
        let y t = m + t in
        y x
      in
      k (5 + m)
    ;;
|}
  in
  helper code;
  [%expect
    {|
  let a c d = 
      let m = (c + d) in
      let k = (fun m l -> 
          let x = (l * 2) in
          let y = (fun m t -> (m + t)) m in
          y x
      ) m in
      k (5 + m)
  ;;
|}]
;;

let%expect_test "factorial cps test" =
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
let fac n = 
    let rec fack n k = if (n <= 1) then 1 else fack (n - 1) ((fun k n m -> k (m * n)) k n) in
    fack n (fun x -> x)
;;
|}]
;;

let%expect_test "closure multi depth test" =
  let code =
    {|
    let f a b = 
      let x y = y + z in
      let t = 
        let g s = m * s in
        g 15
      in
      x 1337
    ;;
|}
  in
  helper code;
  [%expect
    {|
let f = (fun m z a b -> 
    let x = (fun z y -> (y + z)) z in
    let t = 
        let g = (fun m s -> (m * s)) m in
        g 15
     in
    x 1337
) m z;;
|}]
;;
