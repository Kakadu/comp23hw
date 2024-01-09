(** Copyright 2023-2024, Rustam Shangareev and Danil Yevdokimov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

open ZRusML_lib.Parser
open ZRusML_lib.Pretty_printer

let helper code =
  match parse prog code with
  | Error _ -> Printf.printf "PARSE ERROR!"
  | Result.Ok res -> pp_prog Format.std_formatter res
;;

let%expect_test "function test" =
  let code = {|
    let f x = x + y;;
|} in
  helper code;
  [%expect {|
  let f x = (x + y);;
|}]
;;

let%expect_test "let in, apps, operators test" =
  let code =
    {|
    let a c d =
      let m = (c + d = 4) || (d = -7) in
      let z = ((m < 5) >= true) <> (4 / 5 > 0) in
      let abcdef = (c < d) || 5 + 4 in
      let k l = l + m in
      k (5 + m)
    ;;
|}
  in
  helper code;
  [%expect
    {|
let a c d = 
    let m = ((c + d) = 4) || (d = -7) in
    let z = ((m < 5) >= true) <> (4 / 5 > 0) in
    let abcdef = (c < d) || (5 + 4) in
    let k l = (l + m) in
    k (5 + m)
;;
|}]
;;

let%expect_test "let in depth 2, apps, operators" =
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
      let k l = 
          let x = (l * 2) in
          let y t = (m + t) in
          y x
       in
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
    let rec fack n k = if (n <= 1) then 1 else fack (n - 1) (fun m -> k (m * n)) in
    fack n (fun x -> x)
;;
|}]
;;

let%expect_test "let ins test" =
  let code =
    {|
    let f a b = 
      let x y = y + z = 1337 && y = 15 in
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
let f a b = 
    let x y = ((y + z) = 1337) && (y = 15) in
    let t =
        let g s = (m * s) in
        g 15
     in
    x 1337
;;
|}]
;;
