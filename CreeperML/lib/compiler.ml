(** Copyright 2023-2024, Arthur Alekseev and Starcev Matvey *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

module Compiler = struct
  open Std.Std
  open Monad.Result

  let apply_parser = Parser_interface.ParserInterface.from_channel stdin
  let apply_infer = Infer.Infer.top_infer typeenv
  let apply_db_renaming = Indexed_ast.IndexedTypeAst.index_of_typed
  let apply_closure_convert = Closure.ClosureConvert.cf_of_index
  let apply_anf_convert = Anf.AnfConvert.anf_of_cf
  let apply_anf_optimizations = Anf.AnfOptimizations.optimize_moves
  let apply_llvm = Codegen.Codegen.compile

  let cmp f =
    apply_parser >>= apply_infer >>| apply_db_renaming >>| apply_closure_convert
    >>| apply_anf_convert >>| apply_anf_optimizations
    |> function
    | Ok x -> f x
    | Error msg -> print_endline msg

  let dmp_code file =
    cmp (fun x ->
        Codegen.Codegen.top_lvl x |> function
        | Ok _ -> Codegen.Codegen.dmp_code file
        | Error msg -> print_endline msg)

  let compile file = cmp (fun x -> apply_llvm x file)
end
