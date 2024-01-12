(** Copyright 2023-2024, Danila Pechenev, Ilya Dudnikov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open PeDuCoML.Parser
open PeDuCoML.Pprintcf
open PeDuCoML.Closure_conversion
open PeDuCoML.Lambda_lift

let print_ll code =
  match parse code with
  | Ok ast ->
    let closure = run_closure ast in
    let defunced = run_lambda_lifting closure in
    List.iter (fun decl -> Format.printf "%a\n" pp_declaration decl) defunced
  | Error err -> Format.printf "%s\n" err
;;

let _ =
  let code = Stdio.In_channel.input_all Stdlib.stdin in
  print_ll code
;;
