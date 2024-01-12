(** Copyright 2023-2024, Danila Pechenev, Ilya Dudnikov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Util
open Ast

let closure_conversion global_scope =
  let empty = Base.Set.empty (module Base.String) in
  let get_constructor = function
    | DDeclaration _ -> ddeclaration
    | DRecursiveDeclaration _ -> drecursivedeclaration
  in
  let simplify_decl = function
    | ( DDeclaration (name, pattern_list, body)
      | DRecursiveDeclaration (name, pattern_list, body) ) as original ->
      let decl = get_constructor original in
      (match body with
       | EFun (first_arg, other_args, body) ->
         decl name (pattern_list @ (first_arg :: other_args)) body
       | _ -> original)
  in
  let rec closure_declaration env global_scope declaration =
    match simplify_decl declaration with
    | (DDeclaration (name, [], body) | DRecursiveDeclaration (name, [], body)) as original
      ->
      let decl = get_constructor original in
      decl name [] (closure_expression env global_scope body), env, global_scope
    | ( DDeclaration (name, pattern_list, body)
      | DRecursiveDeclaration (name, pattern_list, body) ) as original ->
      let pattern_args =
        List.fold_right
          (fun pattern acc -> Base.Set.union (find_identifiers_pattern pattern) acc)
          pattern_list
          empty
      in
      let pattern_args =
        match original with
        | DDeclaration _ -> pattern_args
        | DRecursiveDeclaration _ -> Base.Set.add pattern_args name
      in
      let free_vars = Base.Set.diff (find_identifiers body) pattern_args in
      let free_vars = Base.Set.diff free_vars global_scope in
      let closed_args =
        Base.Set.fold
          free_vars
          ~f:(fun acc var -> pidentifier var :: acc)
          ~init:pattern_list
      in
      let decl = get_constructor original in
      let env = Base.Map.set env ~key:name ~data:free_vars in
      ( decl name closed_args (closure_expression env global_scope body)
      , env
      , Base.Set.add global_scope name )
  and closure_expression env global_scope = function
    | EFun (first_arg, other_args, body) as original ->
      let pattern_list = first_arg :: other_args in
      let pattern_args =
        List.fold_right
          (fun pattern acc -> Base.Set.union (find_identifiers_pattern pattern) acc)
          pattern_list
          empty
      in
      let free_vars = Base.Set.diff (find_identifiers body) pattern_args in
      let free_vars = Base.Set.diff free_vars global_scope in
      let closed_args =
        Base.Set.fold
          free_vars
          ~f:(fun acc var -> pidentifier var :: acc)
          ~init:pattern_list
      in
      let body = closure_expression env global_scope body in
      (match closed_args with
       | [] -> original
       | head :: tail ->
         Base.Set.fold_right
           free_vars
           ~f:(fun var acc -> eapplication acc (eidentifier var))
           ~init:(efun head tail body))
    | ELetIn (first_decl, other_decls, body) ->
      let decl, env, global_scope = closure_declaration env global_scope first_decl in
      let rec helper decl_list acc global_scope_acc env =
        match decl_list with
        | head :: tail ->
          let head_decl, env, global_scope = closure_declaration env global_scope head in
          helper tail (head_decl :: acc) global_scope env
        | _ -> acc, global_scope_acc
      in
      let decl_acc, global_scope = helper other_decls [] global_scope env in
      eletin decl decl_acc (closure_expression env global_scope body)
    | EIdentifier id as original ->
      (match Base.Map.find env id with
       | None -> original
       | Some free_vars ->
         Base.Set.fold_right
           free_vars
           ~f:(fun var acc -> eapplication acc (eidentifier var))
           ~init:original)
    | EBinaryOperation (bop, left, right) ->
      let left = closure_expression env global_scope left in
      let right = closure_expression env global_scope right in
      ebinary_operation bop left right
    | EUnaryOperation (operator, operand) ->
      let operand = closure_expression env global_scope operand in
      eunary_operation operator operand
    | EApplication (func, arg) ->
      let func = closure_expression env global_scope func in
      let arg = closure_expression env global_scope arg in
      eapplication func arg
    | EList expr_list ->
      let expr_list = Base.List.map expr_list ~f:(closure_expression env global_scope) in
      elist expr_list
    | EConstructList (head, tail) ->
      let head = closure_expression env global_scope head in
      let tail = closure_expression env global_scope tail in
      econstruct_list head tail
    | ETuple (first_elem, second_elem, other_elems) ->
      let first_elem = closure_expression env global_scope first_elem in
      let second_elem = closure_expression env global_scope second_elem in
      let other_elems =
        Base.List.map other_elems ~f:(closure_expression env global_scope)
      in
      etuple first_elem second_elem other_elems
    | EIf (expr, true_branch, false_branch) ->
      let expr = closure_expression env global_scope expr in
      let true_branch = closure_expression env global_scope true_branch in
      let false_branch = closure_expression env global_scope false_branch in
      eif expr true_branch false_branch
    | EMatchWith (matched, first_case, other_cases) ->
      let matched = closure_expression env global_scope matched in
      let first_case =
        fst first_case, closure_expression env global_scope (snd first_case)
      in
      let other_cases =
        Base.List.map other_cases ~f:(fun (pattern, action) ->
          pattern, closure_expression env global_scope action)
      in
      ematchwith matched first_case other_cases
    | expr -> expr
  in
  closure_expression (Base.Map.empty (module Base.String)) global_scope
;;

let run_closure program =
  let get_constructor = function
    | DDeclaration _ -> ddeclaration
    | DRecursiveDeclaration _ -> drecursivedeclaration
  in
  let rec helper acc global_scope = function
    | [] -> acc
    | head :: tail ->
      (match head with
       | ( DDeclaration (id, pattern_list, body)
         | DRecursiveDeclaration (id, pattern_list, body) ) as original ->
         let decl = get_constructor original in
         let body = closure_conversion global_scope body in
         let global_scope = Base.Set.add global_scope id in
         helper (decl id pattern_list body :: acc) global_scope tail)
  in
  Base.List.rev @@ helper [] (Base.Set.empty (module Base.String)) program
;;
