(** Copyright 2023-2024, Grigory Aseev and Matvey Kalashnikov *)

(** SPDX-License-Identifier: LGPL-3.0 *)

open Scaml_lib.Parser
open Scaml_lib.Closure
open Scaml_lib.AstPrinter
open Scaml_lib.Inferencer
open Scaml_lib.Ty
open Base

let print_prog_result prog =
  match parse prog with
  | Ok prog ->
    (match run_prog_inference prog with
     | Ok _ ->
       let prog_closure = prog_conversion prog in
       List.iter prog_closure ~f:(fun binding ->
         Stdlib.Format.printf "%a\n" pp_binding binding)
     | Error e -> print_typ_err e)
  | Error e -> Stdlib.Format.printf "Error%s" e
;;

let () = print_prog_result (Stdio.In_channel.input_all Stdlib.stdin)
