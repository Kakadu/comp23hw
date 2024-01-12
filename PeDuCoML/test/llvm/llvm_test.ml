(** Copyright 2023-2024, Danila Pechenev, Ilya Dudnikov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open PeDuCoML.Parser
open PeDuCoML.Inferencer
open PeDuCoML.Typing
open PeDuCoML.Anf
open PeDuCoML.Closure_conversion
open PeDuCoML.Lambda_lift
open PeDuCoML.Match_elim
open PeDuCoML.Llvm_comp
open PeDuCoML.Pprint_comp_error

let print_llvm code =
  match parse code with
  | Ok ast ->
    (match R.run (check_types ast) with
     | Ok _ ->
       let closure = run_closure ast in
       let defunced = run_lambda_lifting closure in
       let match_free = elim_match defunced in
       let anf = run_anf_conversion match_free in
       (* Base.List.iter
          ~f:(fun func -> Format.printf "%a\n" pp_global_scope_function func)
          anf *)
       (match codegen anf with
        | Ok llvalue_list ->
          Base.List.iter llvalue_list ~f:(fun func ->
            Format.printf "%s" (Llvm.string_of_llvalue func))
        | Error err -> Format.printf "%a\n" pp_error err)
     | Error err -> print_type_error err)
  | Error err -> Format.printf "%s\n" err
;;

let _ =
  let code = Stdio.In_channel.input_all Stdlib.stdin in
  print_llvm code
;;
