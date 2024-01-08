(** Copyright 2023-2024, Ilya Pankratov, Maxim Drumov *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open Anf
open Toplevel
open Ast

(* Simply convert type from const to immexpr *)
let const_to_immexpr = function
  | CInt i -> ImmNum i
  | CBool b -> ImmBool b
;;

(* Maps binary operations from ast.ml to binary operations from anf.ml*)
let binop_to_cexpr_constr op e1 e2 =
  match op with
  | Add -> CPlus (e1, e2)
  | Sub -> CMinus (e1, e2)
  | Div -> CDivide (e1, e2)
  | Mul -> CMultiply (e1, e2)
  | Xor -> CXor (e1, e2)
  | And -> CAnd (e1, e2)
  | Or -> COr (e1, e2)
  | Eq -> CEq (e1, e2)
  | Neq -> CNeq (e1, e2)
  | Gt -> CGt (e1, e2)
  | Lt -> CLt (e1, e2)
  | Gte -> CGte (e1, e2)
  | Lte -> CLte (e1, e2)
;;

(*
   Converts llexpr to aexpr
   Argument expr_with_hole helps to create anf tree in cps
*)
let anf_expr (e : llexpr) (expr_with_hole : immexpr -> aexpr) =
  let open Counter.Counter in
  let fresh name = genid name in
  reset 0;
  let rec helper (e : llexpr) (expr_with_hole : immexpr -> aexpr) =
    match e with
    | LConst (const, _) -> expr_with_hole (const_to_immexpr const)
    | LVar (name, _) -> expr_with_hole (ImmId name)
    | LBinop (op, e1, e2, _) ->
      helper e1 (fun limm ->
        helper e2 (fun rimm ->
          let new_name = fresh "#binop" in
          let op = binop_to_cexpr_constr op in
          ALet (new_name, op limm rimm, expr_with_hole (ImmId new_name))))
    | LApp (e1, e2, _) ->
      helper e1 (fun limm ->
        helper e2 (fun rimm ->
          let new_name = fresh "#app" in
          ALet (new_name, CApp (limm, rimm), expr_with_hole (ImmId new_name))))
    | LLetIn (name, e1, e2, _) | LLetRecIn (name, e1, e2, _) ->
      helper e1 (fun immval -> ALet (name, CImmExpr immval, helper e2 expr_with_hole))
    | LIfThenElse (i, t, e, _) ->
      helper i (fun immif ->
        AIfThenElse
          ( expr_with_hole immif
          , helper t (fun immthen -> expr_with_hole immthen)
          , helper e (fun immelse -> expr_with_hole immelse) ))
  in
  helper e expr_with_hole
;;

(* Performs transformation from llbinding to anfexpr *)
let anf_binding = function
  | (LLet (name, args, expr, _) | LLetRec (name, args, expr, _)) as binding ->
    let binding_to_anf_expr = function
      | LLet _ -> fun name args aexpr -> AnfLet (name, args, aexpr)
      | LLetRec _ -> fun name args aexpr -> AnfLetRec (name, args, aexpr)
    in
    let constructor = binding_to_anf_expr binding in
    let args =
      List.map
        (function
          | Arg (name, _) -> name)
        args
    in
    constructor name args (anf_expr expr (fun imm -> ACEexpr (CImmExpr imm)))
;;

(* Performs transformation from Toplevel.llstatements to Anf.anfstatements *)
let anf lstatements = List.map (fun lbinding -> anf_binding lbinding) lstatements
