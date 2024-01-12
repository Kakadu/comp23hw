open CreeperML
open CreeperML.Parser_interface.ParserInterface
open Infer.Infer
open Indexed_ast.IndexedTypeAst
open Closure.ClosureConvert
open Std
open Pp.PrettyPrinter

let () =
  let program = from_channel stdin in
  let ( >>= ) = Result.bind in
  let apply_db_renaming p = Ok (index_of_typed p) in
  let apply_closure_convert p = Ok (cf_of_index p) in
  let apply_infer p = top_infer Std.typeenv p in
  program >>= apply_infer >>= apply_db_renaming >>= apply_closure_convert
  |> function
  | Ok x -> print_cf_program false x |> print_endline
  | Error x -> print_endline x
