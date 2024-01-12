(** Copyright 2023-2024, Danila Pechenev, Ilya Dudnikov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast

let empty = Base.Set.empty (module Base.String)

let find_identifiers_pattern =
  let rec helper acc = function
    | PIdentifier id -> Base.Set.add acc id
    | PList pattern_list ->
      (match pattern_list with
       | head :: tail -> helper (helper acc head) (PList tail)
       | _ -> acc)
    | PTuple (first_pattern, second_pattern, other_patterns) ->
      helper acc @@ plist (first_pattern :: second_pattern :: other_patterns)
    | PConstructList (operand, list) -> helper (helper acc operand) list
    | _ -> acc
  in
  helper empty
;;

(** Finds all identifiers in an expression. Used in pattern-matching inference *)
let find_identifiers =
  let rec decl_helper acc = function
    | DDeclaration (_, pattern_list, body) | DRecursiveDeclaration (_, pattern_list, body)
      ->
      let pattern_args =
        Base.List.fold_right
          pattern_list
          ~f:(fun pattern acc -> Base.Set.union acc (find_identifiers_pattern pattern))
          ~init:acc
      in
      Base.Set.diff (helper acc body) pattern_args
  and helper acc = function
    | EBinaryOperation (_, left, right) -> helper (helper acc left) right
    | EUnaryOperation (_, operand) -> helper acc operand
    | EApplication (func, arg) -> helper (helper acc func) arg
    | EIdentifier id -> Base.Set.add acc id
    | EFun (first_arg, other_args, body) ->
      let pattern_args =
        Base.List.fold_right
          (first_arg :: other_args)
          ~f:(fun pattern acc -> Base.Set.union acc (find_identifiers_pattern pattern))
          ~init:empty
      in
      Base.Set.diff (helper acc body) pattern_args
    | EList expr_list ->
      (match expr_list with
       | head :: tail -> helper (helper acc head) (elist tail)
       | [] -> acc)
    | EConstructList (head, tail) -> helper (helper acc head) tail
    | ETuple (first_elem, second_elem, other_elems) ->
      helper acc (elist @@ (first_elem :: second_elem :: other_elems))
    | ELetIn (first_decl, other_decls, body) ->
      let acc = decl_helper acc first_decl in
      (match other_decls with
       | head :: tail -> helper acc (eletin head tail body)
       | [] -> helper acc body)
    | EIf (condition, true_branch, false_branch) ->
      let acc = helper acc condition in
      helper (helper acc true_branch) false_branch
    | EMatchWith (matched, (pattern, action), other_cases) ->
      let acc = Base.Set.diff (helper acc action) (find_identifiers_pattern pattern) in
      (match other_cases with
       | head :: tail -> helper acc (ematchwith matched head tail)
       | [] -> helper acc matched)
    | _ -> acc
  in
  helper empty
;;
