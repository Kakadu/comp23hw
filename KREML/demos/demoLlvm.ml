(** Copyright 2023-2024, Anton Kraev and Polina Badreeva *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

let parse_to_llvm program =
  let open KREML_lib in
  let parsed_ast = Parser.parse_optimistically program in
  let llvm =
    parsed_ast
    |> Clos_conv.cc_program
    |> Lambda_lifting.ll_program
    |> Anf_conv.anf_program
    |> Generate_code.llvm_program
  in
  let llvm_string =
    llvm |> List.map  Llvm.string_of_llvalue |> String.concat "\n"
  in
  print_endline llvm_string
;;

let () = parse_to_llvm (Stdio.In_channel.input_all stdin)
