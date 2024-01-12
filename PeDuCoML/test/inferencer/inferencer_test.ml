(** Copyright 2023-2024, Danila Pechenev, Ilya Dudnikov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open PeDuCoML.Parser
open PeDuCoML.Inferencer
open PeDuCoML.Typing

(* Doesn't work yet, this is a template for the future *)
let print_types code =
  match parse code with
  | Ok ast ->
    (match R.run (check_types ast) with
     | Ok typ_list ->
       List.iter
         (fun (name, (_, typ)) -> Format.printf "%s: %a\n" name pp_type typ)
         typ_list
     | Error err -> print_type_error err)
  | Error err -> Format.printf "%s\n" err
;;

let _ =
  let code = Stdio.In_channel.input_all Stdlib.stdin in
  print_types code
;;
