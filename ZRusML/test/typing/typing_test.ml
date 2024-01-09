(** Copyright 2023-2024, Rustam Shangareev and Danil Yevdokimov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

open Base
open ZRusML_lib.Typing

let%test "pp_type prints int type correctly" =
  let buf = Buffer.create 16 in
  let fmt = Format.formatter_of_buffer buf in
  pp_type fmt int_typ;
  Format.pp_print_flush fmt ();
  String.equal (Buffer.contents buf) "int"
;;

let%test "pp_type prints bool type correctly" =
  let buf = Buffer.create 16 in
  let fmt = Format.formatter_of_buffer buf in
  pp_type fmt bool_typ;
  Format.pp_print_flush fmt ();
  String.equal (Buffer.contents buf) "bool"
;;

let%test "pp_type prints arrow type correctly" =
  let buf = Buffer.create 16 in
  let fmt = Format.formatter_of_buffer buf in
  pp_type fmt (arrow_t int_typ bool_typ);
  Format.pp_print_flush fmt ();
  String.equal (Buffer.contents buf) "int -> bool"
;;

let%test "pp_error prints OccursCheck correctly" =
  let buf = Buffer.create 16 in
  let fmt = Format.formatter_of_buffer buf in
  pp_error fmt `OccursCheck;
  Format.pp_print_flush fmt ();
  String.equal (Buffer.contents buf) "Occurs check failed."
;;
