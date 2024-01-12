(** Copyright 2023-2024, Danila Pechenev, Ilya Dudnikov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast

type match_free_expr =
  | MFLiteral of literal
  | MFBinaryOperation of binary_operator * match_free_expr * match_free_expr
  | MFUnaryOperation of unary_operator * match_free_expr
  | MFApplication of match_free_expr * match_free_expr
  | MFIdentifier of id
  | MFList of match_free_expr list
  | MFConstructList of match_free_expr * match_free_expr
  | MFLetIn of match_free_let * match_free_let list * match_free_expr
  | MFTuple of match_free_expr * match_free_expr * match_free_expr list
  | MFIf of match_free_expr * match_free_expr * match_free_expr

and match_free_let = id * match_free_expr

type match_free_decl =
  | MFDeclaration of id * id list * match_free_expr
  | MFRecursiveDeclaration of id * id list * match_free_expr

let mfliteral literal = MFLiteral literal
let mfbinaryoperation bop left right = MFBinaryOperation (bop, left, right)
let mfunaryoperation operator operand = MFUnaryOperation (operator, operand)
let mfapplication func arg = MFApplication (func, arg)
let mfidentifier id = MFIdentifier id
let mflist list = MFList list
let mfconstructlist head tail = MFConstructList (head, tail)
let mfletin first_let other_lets expr = MFLetIn (first_let, other_lets, expr)

let mftuple first_elem second_elem other_elems =
  MFTuple (first_elem, second_elem, other_elems)
;;

let mfif cond true_branch false_branch = MFIf (cond, true_branch, false_branch)
let mflet id body = id, body
let mfdeclaration name arguments body = MFDeclaration (name, arguments, body)

let mfrecursivedeclaration name arguments body =
  MFRecursiveDeclaration (name, arguments, body)
;;

open Lambda_lift

let rec rewrite_match expr =
  let rt_tail = mfapplication (mfidentifier "`peducoml_tail") in
  let rt_length = mfapplication (mfidentifier "`peducoml_list_length") in
  let rt_list_field lst idx =
    mfapplication
      (mfapplication (mfidentifier "`peducoml_list_field") lst)
      (mfliteral @@ lint idx)
  in
  let rt_tuple_field lst idx =
    mfapplication
      (mfapplication (mfidentifier "`peducoml_tuple_field") lst)
      (mfliteral @@ lint idx)
  in
  let rt_head lst = rt_list_field lst 0 in
  match expr with
  | CFIdentifier id -> mfidentifier id
  | CFLiteral literal -> mfliteral literal
  | CFBinaryOperation (bop, left, right) ->
    let left_rewritten = rewrite_match left in
    let right_rewritten = rewrite_match right in
    mfbinaryoperation bop left_rewritten right_rewritten
  | CFUnaryOperation (uop, operand) ->
    let operand_rewritten = rewrite_match operand in
    mfunaryoperation uop operand_rewritten
  | CFApplication (fun_expr, arg_expr) ->
    let fun_expr_rewritten = rewrite_match fun_expr in
    let arg_expr_rewritten = rewrite_match arg_expr in
    mfapplication fun_expr_rewritten arg_expr_rewritten
  | CFList expr_list -> mflist @@ List.map (fun e -> rewrite_match e) expr_list
  | CFTuple (first_elem, second_elem, other_elems) ->
    let first_elem_rewritten = rewrite_match first_elem in
    let second_elem_rewritten = rewrite_match second_elem in
    mftuple first_elem_rewritten second_elem_rewritten
    @@ List.map (fun e -> rewrite_match e) other_elems
  | CFIf (condition, true_branch, false_branch) ->
    let condition_rewritten = rewrite_match condition in
    let true_branch_rewritten = rewrite_match true_branch in
    let false_branch_rewritten = rewrite_match false_branch in
    mfif condition_rewritten true_branch_rewritten false_branch_rewritten
  | CFConstructList (operand, expr_list) ->
    let operand_rewritten = rewrite_match operand in
    let expr_list_rewritten = rewrite_match expr_list in
    mfconstructlist operand_rewritten expr_list_rewritten
  | CFLetIn (first_declaration, other_declarations, body) ->
    let rewrite_let (id, body) = mflet id (rewrite_match body) in
    mfletin
      (rewrite_let first_declaration)
      (List.map rewrite_let other_declarations)
      (rewrite_match body)
  | CFMatchWith (matched_expression, first_case, other_cases) ->
    let cases = first_case :: other_cases in
    let concat_conditions first second =
      match first, second with
      | MFLiteral (LBool true), second -> second
      | first, MFLiteral (LBool true) -> first
      | _ -> mfbinaryoperation AND first second
    in
    let rec get_match_condition matched_expression = function
      | PLiteral literal -> mfbinaryoperation Eq matched_expression (mfliteral literal)
      | PList pattern_list ->
        let pattern_list_length = List.length pattern_list in
        let partial_condition =
          mfif
            (mfbinaryoperation
               NEq
               (rt_length matched_expression)
               (mfliteral @@ lint pattern_list_length))
            (mfliteral @@ lbool false)
        in
        let elems_condition =
          Base.List.fold_left
            ~init:(mfliteral @@ lbool true, 0)
            pattern_list
            ~f:(fun (cond, ind) pattern ->
              let pattern_condition =
                get_match_condition (rt_list_field matched_expression ind) pattern
              in
              concat_conditions cond pattern_condition, ind + 1)
        in
        partial_condition (fst elems_condition)
      | PTuple (first_pattern, second_pattern, pattern_list) ->
        let pattern_list = first_pattern :: second_pattern :: pattern_list in
        let elems_condition =
          Base.List.fold
            ~init:(mfliteral @@ lbool true, 0)
            pattern_list
            ~f:(fun (cond, ind) pattern ->
              let pattern_condition =
                get_match_condition (rt_tuple_field matched_expression ind) pattern
              in
              concat_conditions cond pattern_condition, ind + 1)
        in
        fst elems_condition
      | PConstructList (head_pattern, tail_pattern) ->
        let partial_condition =
          mfif
            (mfbinaryoperation Eq (rt_length matched_expression) (mfliteral @@ lint 0))
            (mfliteral @@ lbool false)
        in
        let head_condition =
          get_match_condition (rt_head matched_expression) head_pattern
        in
        let tail_condition =
          get_match_condition (rt_tail matched_expression) tail_pattern
        in
        partial_condition @@ concat_conditions head_condition tail_condition
      | _ -> mfliteral @@ lbool true
    in
    let rec rewrite_pattern matched pattern action =
      match pattern with
      | PIdentifier id -> mfletin (mflet id matched) [] action
      | PTuple (first_elem, second_elem, other_elems) ->
        let elem_list = first_elem :: second_elem :: other_elems in
        let rewritten, _ =
          Base.List.fold elem_list ~init:(action, 0) ~f:(fun (action, ind) elem ->
            rewrite_pattern (rt_tuple_field matched ind) elem action, ind + 1)
        in
        rewritten
      | PList pattern_list ->
        let rewritten, _ =
          Base.List.fold pattern_list ~init:(action, 0) ~f:(fun (action, ind) elem ->
            rewrite_pattern (rt_list_field matched ind) elem action, ind + 1)
        in
        rewritten
      | PConstructList (head, tail) ->
        rewrite_pattern (rt_head matched) head
        @@ rewrite_pattern (rt_tail matched) tail action
      | _ -> action
    in
    let rec gen_conditions k cases =
      let pattern, action = Base.List.hd_exn cases in
      let action = rewrite_match action in
      let matched_expression = rewrite_match matched_expression in
      match Base.List.tl_exn cases with
      | [] -> k @@ rewrite_pattern matched_expression pattern action
      | tail ->
        gen_conditions
          (fun else_branch ->
            k
            @@ mfif
                 (get_match_condition matched_expression pattern)
                 (rewrite_pattern matched_expression pattern action)
                 else_branch)
          tail
    in
    gen_conditions (fun id -> id) cases
;;

let elim_match program =
  Base.List.map
    program
    ~f:
      (let get_constructor = function
         | CFDeclaration _ -> mfdeclaration
         | CFRecursiveDeclaration _ -> mfrecursivedeclaration
       in
       function
       | (CFDeclaration (id, id_list, body) | CFRecursiveDeclaration (id, id_list, body))
         as original ->
         let decl = get_constructor original in
         decl id id_list (rewrite_match body))
;;
