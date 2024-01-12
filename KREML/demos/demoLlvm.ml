(** Copyright 2023-2024, Anton Kraev and Polina Badreeva *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

let parse_to_llvm program =
  let open KREML_lib in
  let ast = Parser.parse_optimistically program in
  let typecheck = Inferencer.run_inference ast in
  let result =
    match typecheck with
    | Ok typ ->
      (match typ with
       | Typing.TArr _ -> Typing.print_type typ
       | _ ->
         ast
         |> Clos_conv.cc_program
         |> Lambda_lifting.ll_program
         |> Anf_conv.anf_program
         |> Generate_code.llvm_program
         |> List.map Llvm.string_of_llvalue
         |> String.concat "\n"
         |> print_endline)
    | Error err -> Typing.print_type_error err
  in
  result
;;

let () = parse_to_llvm (Stdio.In_channel.input_all stdin)
