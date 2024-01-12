(** Copyright 2023-2024, Danila Pechenev, Ilya Dudnikov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open PeDuCoML.Parser
open PeDuCoML.Pprintast
open PeDuCoML.Closure_conversion
open PeDuCoML.Inferencer
open PeDuCoML.Typing

let print_closure_free code =
  match parse code with
  | Ok ast ->
    (match R.run (check_types ast) with
     | Ok _ ->
       let closure = run_closure ast in
       List.iter (fun decl -> Format.printf "%a\n" pp_declaration decl) closure
     | Error err -> print_type_error err)
  | Error err -> Format.printf "%s\n" err
;;

let _ =
  let code = Stdio.In_channel.input_all Stdlib.stdin in
  print_closure_free code
;;
