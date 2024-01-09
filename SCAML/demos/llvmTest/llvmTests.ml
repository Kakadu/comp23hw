(** Copyright 2023-2024, Grigory Aseev and Matvey Kalashnikov *)

(** SPDX-License-Identifier: LGPL-3.0 *)

open Scaml_lib.Parser
open Scaml_lib.Closure
open Scaml_lib.Inferencer
open Scaml_lib.Ty
open Scaml_lib.LambdaLifting
open Scaml_lib.Anf
open Scaml_lib.LlvmCompiler
open Base

let print_prog_result prog =
  match parse prog with
  | Ok prog ->
    (match run_prog_inference prog with
     | Ok _ ->
       let prog_closure = prog_conversion prog in
       let lifted = run_ll prog_closure in
       let anf_prog = anf_program lifted in
       (match codegen_program anf_prog with
        | Ok llvalue_list ->
          Base.List.iter llvalue_list ~f:(fun f ->
            Stdlib.Format.printf "%s\n" (Llvm.string_of_llvalue f))
        | Error e -> Stdlib.Format.printf "Error%s" e)
     | Error e -> print_typ_err e)
  | Error e -> Stdlib.Format.printf "Error%s" e
;;

let () = print_prog_result (Stdio.In_channel.input_all Stdlib.stdin)
