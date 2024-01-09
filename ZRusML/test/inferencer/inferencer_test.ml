(** Copyright 2023-2024, Rustam Shangareev and Danil Yevdokimov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

open ZRusML_lib.Code_inferencer

let%expect_test "arithmetic operations test" =
  let code =
    {|
    let x = 3 * 4 / 2 + 7;;
    let y = 15 + 7 / 2;;
    let result = x + y * x;;
  |}
  in
  Format.printf "%a" inference code;
  [%expect {|
    val x : int
    val y : int
    val result : int
  |}]
;;

let%expect_test "let rec test" =
  let code =
    {|
    let rec timer = fun y -> if y = 0 then 0 else timer (y - 1);;
    let tmp = timer 17;;
  |}
  in
  Format.printf "%a" inference code;
  [%expect {|
    val timer : int -> int
    val tmp : int
  |}]
;;

let%expect_test "binary operator inference test" =
  let code = {|
    let e = fun x -> (if x >= 2 then x = 15 else false);;
  |} in
  Format.printf "%a" inference code;
  [%expect {|
    val e : int -> bool
  |}]
;;

let%expect_test "typs test" =
  let code = {|
    let int = 3;;
    let bool = true;;
  |} in
  Format.printf "%a" inference code;
  [%expect {|
    val int : int
    val bool : bool
  |}]
;;

let%expect_test "factorial test" =
  let code = {|
    let rec fact n = if n = 0 then 1 else n * (fact (n - 1));;
  |} in
  Format.printf "%a" inference code;
  [%expect {|
    val fact : int -> int
  |}]
;;

let%expect_test "factorial acc test" =
  let code =
    {|
    let rec acc_fact acc n = if n = 0 then acc else acc_fact (acc * n) (n - 1);;
    let test = acc_fact 1 3 = 6;;
  |}
  in
  Format.printf "%a" inference code;
  [%expect {|
    val acc_fact : int -> int -> int
    val test : bool
  |}]
;;

let%expect_test "unbound value test" =
  let code = {|
    let rec x y = x (y - 1);;
    let z = y;;
  |} in
  Format.printf "%a" inference code;
  [%expect
    {|
Error in №2 declaration:
Elaboration failed: Unbound value identifier y
  |}]
;;

let%expect_test "unifaction error test" =
  let code = {|
    let y = 4;;
    let x = true;;
    let z = x = y;;
  |} in
  Format.printf "%a" inference code;
  [%expect
    {|
Error in №3 declaration:
Elaboration failed: Rules disagree on type: Cannot merge int and bool
  |}]
;;

let%expect_test "parse error test" =
  let code = {|
    let x
  |} in
  Format.printf "%a" inference code;
  [%expect {|
Parse error
  |}]
;;
