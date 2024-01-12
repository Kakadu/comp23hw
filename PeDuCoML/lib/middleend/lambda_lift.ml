(** Copyright 2023-2024, Danila Pechenev, Ilya Dudnikov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open State
open Util

type cf_expr =
  | CFLiteral of literal
  | CFBinaryOperation of binary_operator * cf_expr * cf_expr
  | CFUnaryOperation of unary_operator * cf_expr
  | CFApplication of cf_expr * cf_expr
  | CFIdentifier of id
  | CFList of cf_expr list
  | CFConstructList of cf_expr * cf_expr
  | CFLetIn of cf_let * cf_let list * cf_expr
  | CFTuple of cf_expr * cf_expr * cf_expr list
  | CFMatchWith of cf_expr * (pattern * cf_expr) * (pattern * cf_expr) list
  | CFIf of cf_expr * cf_expr * cf_expr

and cf_let = id * cf_expr

type cf_decl =
  | CFDeclaration of id * id list * cf_expr
  | CFRecursiveDeclaration of id * id list * cf_expr

let cfliteral literal = CFLiteral literal
let cfbinaryoperation bop left right = CFBinaryOperation (bop, left, right)
let cfunaryoperation operator operand = CFUnaryOperation (operator, operand)
let cfapplication func arg = CFApplication (func, arg)
let cfidentifier id = CFIdentifier id
let cflist list = CFList list
let cfconstructlist head tail = CFConstructList (head, tail)
let cfletin first_let other_lets expr = CFLetIn (first_let, other_lets, expr)

let cftuple first_elem second_elem other_elems =
  CFTuple (first_elem, second_elem, other_elems)
;;

let cfmatchwith matched first_case other_cases =
  CFMatchWith (matched, first_case, other_cases)
;;

let cfif cond true_branch false_branch = CFIf (cond, true_branch, false_branch)
let cflet id body = id, body
let cfdeclaration name arguments body = CFDeclaration (name, arguments, body)

let cfrecursivedeclaration name arguments body =
  CFRecursiveDeclaration (name, arguments, body)
;;

let lambda_lift =
  let eliminate_arg_patterns body pattern_list =
    let generate_unique_arg_name used_names start =
      let start_number = Base.Set.length used_names in
      let rec find_unique current_number =
        let current_name = start ^ string_of_int current_number in
        match Base.Set.find used_names ~f:(fun elem -> elem = current_name) with
        | None -> current_name
        | _ -> find_unique (current_number + 1)
      in
      find_unique start_number
    in
    let rec helper used_names current_expr args_list = function
      | head :: tail ->
        (match head with
         | PIdentifier id ->
           helper (Base.Set.add used_names id) current_expr (id :: args_list) tail
         | _ ->
           let arg_name = generate_unique_arg_name used_names "arg_" in
           helper
             (Base.Set.add used_names arg_name)
             (ematchwith (eidentifier arg_name) (head, current_expr) [])
             (arg_name :: args_list)
             tail)
      | _ -> List.rev args_list, current_expr
    in
    let used_names = find_identifiers body in
    helper used_names body [] pattern_list
  in
  let ll_expr expr =
    let get_constructor = function
      | DDeclaration _ -> cfdeclaration
      | DRecursiveDeclaration _ -> cfrecursivedeclaration
    in
    let rec ll_decl env lifted_decls = function
      | ( DDeclaration (name, head :: tail, body)
        | DRecursiveDeclaration (name, head :: tail, body) ) as original ->
        let pattern_list = head :: tail in
        let* fresh_var = fresh in
        let fresh_name = "`ll_" ^ string_of_int fresh_var in
        let env = Base.Map.set env ~key:name ~data:fresh_name in
        let args_list, body = eliminate_arg_patterns body pattern_list in
        let* body, lifted = ll_expr env lifted_decls body in
        let decl = get_constructor original in
        return
        @@ ( name
           , fresh_name
           , cflet fresh_name body
           , decl fresh_name args_list body :: lifted )
      | DDeclaration (name, _, body) | DRecursiveDeclaration (name, _, body) ->
        let* body, lifted = ll_expr env lifted_decls body in
        return @@ (name, name, cflet name body, lifted)
    and ll_expr env lifted_decls = function
      | EFun (first_arg, other_args, body) ->
        let* fresh_var = fresh in
        let id = "`ll_" ^ string_of_int fresh_var in
        let args_list, body = eliminate_arg_patterns body (first_arg :: other_args) in
        let* body, lifted = ll_expr env lifted_decls body in
        return @@ (cfidentifier id, cfdeclaration id args_list body :: lifted)
      | ELetIn (first_decl, other_decls, body) ->
        let* old_name, fresh_name, first, lifted = ll_decl env lifted_decls first_decl in
        let decl_acc = if old_name = fresh_name then [ first ] else [] in
        let* env, lifted, decl_acc =
          Base.List.fold_right
            other_decls
            ~f:(fun decl acc ->
              let* env, lifted, decl_acc = acc in
              let* old_name, fresh_name, decl, lifted = ll_decl env lifted decl in
              let decl_acc =
                if old_name = fresh_name then decl :: decl_acc else decl_acc
              in
              return @@ (Base.Map.set env ~key:old_name ~data:fresh_name, lifted, decl_acc))
            ~init:
              (return (Base.Map.set env ~key:old_name ~data:fresh_name, lifted, decl_acc))
        in
        let* body, lifted = ll_expr env lifted body in
        (match decl_acc with
         | head :: tail -> return @@ (cfletin head tail body, lifted)
         | [] -> return (body, lifted))
      | EBinaryOperation (bop, left, right) ->
        let* left, lifted = ll_expr env lifted_decls left in
        let* right, lifted = ll_expr env lifted right in
        return @@ (cfbinaryoperation bop left right, lifted)
      | EUnaryOperation (operation, operand) ->
        let* operand, lifted = ll_expr env lifted_decls operand in
        return @@ (cfunaryoperation operation operand, lifted)
      | EApplication (func, arg) ->
        let* func, lifted = ll_expr env lifted_decls func in
        let* arg, lifted = ll_expr env lifted arg in
        return @@ (cfapplication func arg, lifted)
      | EList expr_list ->
        let* expr_list, lifted =
          Base.List.fold_right
            expr_list
            ~f:(fun expr acc ->
              let* expr_acc, lifted_acc = acc in
              let* expr, lifted_from_expr = ll_expr env lifted_acc expr in
              return @@ (expr :: expr_acc, lifted_from_expr))
            ~init:(return ([], lifted_decls))
        in
        return @@ (cflist expr_list, lifted)
      | EConstructList (head, tail) ->
        let* head, lifted = ll_expr env lifted_decls head in
        let* tail, lifted = ll_expr env lifted tail in
        return @@ (cfconstructlist head tail, lifted)
      | ETuple (first_elem, second_elem, other_elems) ->
        let* first, lifted = ll_expr env lifted_decls first_elem in
        let* second, lifted = ll_expr env lifted second_elem in
        let* other_elems, lifted =
          Base.List.fold_right
            other_elems
            ~f:(fun expr acc ->
              let* expr_acc, lifted_acc = acc in
              let* expr, lifted_from_expr = ll_expr env lifted_acc expr in
              return @@ (expr :: expr_acc, lifted_from_expr))
            ~init:(return ([], lifted))
        in
        return @@ (cftuple first second other_elems, lifted)
      | EIf (condition, true_branch, false_branch) ->
        let* condition, lifted = ll_expr env lifted_decls condition in
        let* true_branch, lifted = ll_expr env lifted true_branch in
        let* false_branch, lifted = ll_expr env lifted false_branch in
        return @@ (cfif condition true_branch false_branch, lifted)
      | EMatchWith (matched, (first_pattern, first_action), other_cases) ->
        let* matched, lifted = ll_expr env lifted_decls matched in
        let* first_action, lifted = ll_expr env lifted first_action in
        let* other_cases, lifted =
          Base.List.fold_right
            other_cases
            ~f:(fun (pattern, action) acc ->
              let* case_acc, lifted_acc = acc in
              let* action, lifted = ll_expr env lifted_acc action in
              return @@ ((pattern, action) :: case_acc, lifted))
            ~init:(return ([], lifted))
        in
        return @@ (cfmatchwith matched (first_pattern, first_action) other_cases, lifted)
      | EIdentifier id ->
        (match Base.Map.find env id with
         | None -> return @@ (cfidentifier id, lifted_decls)
         | Some new_id -> return @@ (cfidentifier new_id, lifted_decls))
      | ELiteral literal -> return @@ (cfliteral literal, lifted_decls)
    in
    ll_expr (Base.Map.empty (module Base.String)) [] expr
  in
  let rec ll_program =
    let get_constructor = function
      | DDeclaration _ -> cfdeclaration
      | DRecursiveDeclaration _ -> cfrecursivedeclaration
    in
    function
    | [] -> return []
    | head :: tail ->
      (match head with
       | ( DDeclaration (name, pattern_list, body)
         | DRecursiveDeclaration (name, pattern_list, body) ) as original ->
         let decl = get_constructor original in
         let args_list, body = eliminate_arg_patterns body pattern_list in
         let* body, lifted_decls = ll_expr body in
         let* tail = ll_program tail in
         return @@ tail @ (decl name args_list body :: lifted_decls))
  in
  ll_program
;;

let run_lambda_lifting program = List.rev @@ run (lambda_lift program)
