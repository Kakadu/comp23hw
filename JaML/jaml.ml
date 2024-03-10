(** Copyright 2023-2024, Ilya Pankratov, Maxim Drumov *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open Jaml_lib
open Format
open Result

module JamlCLIArgs = struct
  type t =
    { infer_type : bool
    ; filename : string
    ; occurs_check : bool
    ; compiler : bool
    }

  let usage = "jaml -i -d -f -ll -x86_64 <file>\nCompile program from stdin"

  let parse () =
    let infer = ref false in
    let occurs_check_flag = ref true in
    let file = ref "" in
    let compiler = ref false in
    let specs =
      [ ( "-i"
        , Arg.Set infer
        , "Infer only the types for the input, do not use the compiler." )
      ; "-f", Set_string file, "Read program from specified file, not from the stdin."
      ; "-d", Set occurs_check_flag, "Disable occurrence checking during type checking"
      ; "-ll", Clear compiler, "Compilation with Llvm"
      ; "-x86_64", Set compiler, "Compilation with x86_64"
      ]
    in
    let anon _ = () in
    Arg.parse specs anon usage;
    { infer_type = !infer
    ; filename = !file
    ; occurs_check = not !occurs_check_flag
    ; compiler = !compiler
    }
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
    (match Inferencer.infer mode statements with
     | Ok typed_statements -> printf "%a\n" pp_statements typed_statements
     | Error err -> printf "%a\n" Inferencer.pp_error err)
  | Error err -> printf "%a\n" Parser.pp_error err
;;

let compile_llvm test_case mode =
  let fmt = Format.std_formatter in
  match Parser.parse test_case with
  | Error err -> Parser.pp_error fmt err
  | Ok commands ->
    (match Inferencer.infer mode commands with
     | Error err -> Inferencer.pp_error fmt err
     | Ok typed_commands ->
       Closure.closure typed_commands
       |> Lambdalift.lambda_lift
       |> Anfconv.anf
       |> Codegen.compile
       |> fun llvalue_list ->
       Base.List.iter llvalue_list ~f:(fun f ->
         Stdlib.Format.printf "%s\n" (Llvm.string_of_llvalue f)))
;;

let compile_asm test_case mode =
  let fmt = Format.std_formatter in
  match Parser.parse test_case with
  | Error err -> Parser.pp_error fmt err
  | Ok commands ->
    (match Inferencer.infer mode commands with
     | Error err -> Inferencer.pp_error fmt err
     | Ok typed_commands ->
       Closure.closure typed_commands
       |> Lambdalift.lambda_lift
       |> Anfconv.anf
       |> X86_64.assembly
       |> (function
        | Ok asm ->
          Monads.CompilerMonad.commands_to_file "jaml.S" asm;
          let _ =
            Sys.command
              "nasm -f elf64 -o jaml.o jaml.S && gcc -o jaml jaml.o lib/runtime.o \
               lib/stdlib.o 2> /dev/null"
          in
          ()
        | _ -> failwith ""))
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
  let args = JamlCLIArgs.parse () in
  let mode = flag_to_occurs_check_mode args.occurs_check in
  match read_input args.filename with
  | Ok input when input <> "" && args.infer_type -> infer_types input mode
  | Ok input when input <> "" && args.compiler && not args.infer_type ->
    compile_asm input mode
  | Ok input when input <> "" && (not args.compiler) && not args.infer_type ->
    compile_llvm input mode
  | Ok _ -> printf "empty input"
  | Error err -> printf "%a\n" pp_error err
;;
