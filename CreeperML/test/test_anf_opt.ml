open CreeperML
open CreeperML.Parser_interface.ParserInterface
open Infer.Infer
open Type_ast.InferTypeUtils
open Type_ast.TypeAst
open Counter
open Pp.PrettyPrinter
open Indexed_ast.IndexedTypeAst
open Closure.ClosureConvert
open String
open Anf.AnfConvert
open Anf.AnfOptimizations
module NameMap = Map.Make (String)

let pi =
  let int_const = t_ground t_int |> with_lvls 0 0 in
  let unit_const = t_ground t_unit |> with_lvls 0 0 in
  let pi = t_arrow int_const unit_const |> with_lvls 0 0 in
  ("print_int", pi)

let lr =
  let int_const = t_ground t_int |> with_lvls 0 0 in
  let bool_const = t_ground t_bool |> with_lvls 0 0 in
  let arr = t_arrow int_const bool_const |> with_lvls 0 0 in
  let lr = t_arrow int_const arr |> with_lvls 0 0 in
  ("<=", lr)

let mi =
  let int_const = t_ground t_int |> with_lvls 0 0 in
  let arr = t_arrow int_const int_const |> with_lvls 0 0 in
  let mi = t_arrow int_const arr |> with_lvls 0 0 in
  ("-", mi)

let ml =
  let int_const = t_ground t_int |> with_lvls 0 0 in
  let arr = t_arrow int_const int_const |> with_lvls 0 0 in
  let ml = t_arrow int_const arr |> with_lvls 0 0 in
  ("*", ml)

let pl =
  let int_const = t_ground t_int |> with_lvls 0 0 in
  let arr = t_arrow int_const int_const |> with_lvls 0 0 in
  let pl = t_arrow int_const arr |> with_lvls 0 0 in
  ("+", pl)

let nm =
  let nm = NameMap.empty in
  let nm = NameMap.add "<=" (cnt_next ()) nm in
  let nm = NameMap.add "-" (cnt_next ()) nm in
  let nm = NameMap.add "*" (cnt_next ()) nm in
  let nm = NameMap.add "+" (cnt_next ()) nm in
  let nm = NameMap.add "print_int" (cnt_next ()) nm in
  nm

let env = [ lr; mi; ml; pi; pl ]
let typed t a : ('a, ty) typed = { value = a; typ = t }

let globals =
  [
    typed (TyArrow (TyArrow (TyGround TInt, TyGround TInt), TyGround TBool))
    @@ NameMap.find "<=" nm;
    typed (TyArrow (TyArrow (TyGround TInt, TyGround TInt), TyGround TInt))
    @@ NameMap.find "-" nm;
    typed (TyArrow (TyArrow (TyGround TInt, TyGround TInt), TyGround TInt))
    @@ NameMap.find "*" nm;
    typed (TyArrow (TyArrow (TyGround TInt, TyGround TInt), TyGround TInt))
    @@ NameMap.find "+" nm;
    typed (TyArrow (TyGround TInt, TyGround TUnit))
    @@ NameMap.find "print_int" nm;
  ]

(*
  Used functions and operators:
    1: <= (i -> i -> b)
    2: -  (i -> i -> i)
    3: *  (i -> i -> i)
    4: +  (i -> i -> i)
    5: print_int (i -> ())
*)

let () =
  match from_channel stdin with
  | Error msg -> Printf.printf "%s" msg
  | Ok p -> (
      match top_infer env p with
      | Error msg -> Printf.printf "%s" msg
      | Ok p ->
          index_of_typed nm p |> cf_of_index globals |> anf_of_cf
          |> optimize_moves |> print_anf_program false |> trim |> print_endline)
