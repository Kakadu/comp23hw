(** Copyright 2023-2024, Mikhail Vyrodov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Neinml_lib

let () =
  let s = Stdio.In_channel.input_all Stdlib.stdin in
  match Neinml_lib.Parser.parse s with
  | Result.Ok ast -> Format.printf "%a\n%!" Ast.pp_statements_list ast
  | Error _ -> Format.printf "Parsing error"
;;
