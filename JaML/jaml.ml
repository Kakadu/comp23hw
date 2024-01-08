(** Copyright 2023-2024, Ilya Pankratov, Maxim Drumov *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open Jaml_lib
open Format
open Result

module YamlCLIArgs = struct
  type t =
    { infer_type : bool
    ; filename : string
    ; occurs_check : bool
    }

  let usage = "jaml -i -d -f <file>\nCompile program from stdin"

  let parse () =
    let infer = ref false in
    let occurs_check_flag = ref true in
    let file = ref "" in
    let specs =
      [ ( "-i"
        , Arg.Set infer
        , "Infer only the types for the input, do not use the compiler." )
      ; "-f", Set_string file, "Read program from specified file, not from the stdin."
      ; "-d", Set occurs_check_flag, "Disable occurrence checking during type checking"
      ]
    in
    let anon _ = () in
    Arg.parse specs anon usage;
    { infer_type = !infer; filename = !file; occurs_check = not !occurs_check_flag }
  ;;
end

type errors = FileDoesNotExist of string

let pp_error ppf = function
  | FileDoesNotExist file -> fprintf ppf "file '%s' does not exist" file
;;

let pp_statements = Pprinttypedtree.pp_statements "\n" Pprinttypedtree.Brief

let flag_to_occurs_check_mode flag =
  if flag then Inferencer.Enable else Inferencer.Disable
;;

let infer_types input mode =
  let parsed = Parser.parse input in
  match parsed with
  | Ok statements ->
    (match Inferencer.infer statements mode with
     | Ok typed_statements -> printf "%a\n" pp_statements typed_statements
     | Error err -> printf "%a\n" Inferencer.pp_error err)
  | Error err -> printf "%a\n" Parser.pp_error err
;;

let read_input filename =
  if filename <> ""
  then
    if Sys.file_exists filename
    then Ok (In_channel.with_open_bin filename In_channel.input_all)
    else Error (FileDoesNotExist filename)
  else Ok (In_channel.input_all In_channel.stdin)
;;

let () =
  let args = YamlCLIArgs.parse () in
  let mode = flag_to_occurs_check_mode args.occurs_check in
  match read_input args.filename with
  | Ok input when input <> "" && args.infer_type -> infer_types input mode
  | Ok input when input <> "" && not args.infer_type ->
    printf "compilation is not supported"
  | Ok _ -> printf "empty input"
  | Error err -> printf "%a\n" pp_error err
;;
