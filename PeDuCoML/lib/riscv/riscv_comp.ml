(** Copyright 2023-2024, Danila Pechenev, Ilya Dudnikov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Anf
open Ast
open Arg_counter
open Result
open Riscv

let ( >>= ) = bind
let ( >>| ) m f = m >>= fun x -> ok (f x)

type riscv_error = UnboundVariable of unique_id

let unbound id = UnboundVariable id

module Environment = struct
  let find = Base.Map.Poly.find
  let empty = Base.Map.Poly.empty
  let set = Base.Map.Poly.set
end

let ( let* ) = bind

open Environment

let build_binary_operation = function
  | Add -> build_add
  | Sub -> build_sub
  | Mul -> build_mul
  | Div -> build_div
  | AND -> build_and
  | OR -> build_or
  | Eq -> build_eq
  | NEq -> build_neq
  | LT -> build_lt
  | GTE -> build_gte
  | LTE -> build_lte
  | GT -> build_gt
;;

let build_unary_operation = function
  | Minus -> build_neg
  | Not -> build_not
;;

let rec codegen_immexpr args_number env =
  let list_helper imm_list =
    let allocated_list = build_call (lookup_function_exn "peducoml_alloc_list") [] in
    let add = lookup_function_exn "peducoml_add_to_list" in
    Base.List.fold (Base.List.rev imm_list) ~init:(ok allocated_list) ~f:(fun acc elem ->
      let* elem, _ = codegen_immexpr args_number env elem in
      let* acc = acc in
      ok @@ build_call add [ acc; elem ])
  in
  function
  | ImmInt num -> ok (const_int num, 0)
  | ImmChar c -> ok (Base.Char.to_int c |> const_int, 0)
  | ImmBool v -> ok (Base.Bool.to_int v |> const_int, 0)
  | ImmId (GlobalScopeId id) ->
    let func = lookup_function_exn id in
    let args = params func |> Option.get in
    if args > 0
    then ok (func, args)
    else ok @@ (build_call (lookup_function_exn "peducoml_apply0") [ func ], args)
  | ImmId (AnfId id) ->
    (match find env (AnfId id) with
     | None -> AnfId id |> unbound |> error
     | Some value ->
       let number_of_args =
         match find args_number (AnfId id) with
         | Some n -> n
         | None -> 0
       in
       ok (value, number_of_args))
  | ImmString str ->
    let* lst =
      Base.String.to_list str |> Base.List.map ~f:(fun x -> ImmChar x) |> list_helper
    in
    ok (lst, 0)
  | ImmList imm_list ->
    let* lst = list_helper imm_list in
    ok (lst, 0)
  | ImmTuple imm_list ->
    let allocated_tuple =
      build_call
        (lookup_function_exn "peducoml_alloc_tuple")
        [ Base.List.length imm_list |> const_int ]
    in
    let* result =
      Base.List.fold imm_list ~init:(ok allocated_tuple) ~f:(fun acc imm ->
        let* elem, _ = codegen_immexpr args_number env imm in
        let* acc = acc in
        ok @@ build_call (lookup_function_exn "peducoml_fill_tuple") [ acc; elem ])
    in
    ok (result, 0)
;;

let rec codegen_cexpr args_number env = function
  | CImm immexpr -> codegen_immexpr args_number env immexpr >>| fst
  | CBinaryOperation (bop, left_imm, right_imm) ->
    let* left, _ = codegen_immexpr args_number env left_imm in
    let* right, _ = codegen_immexpr args_number env right_imm in
    (match bop with
     | Div -> ok @@ build_call (lookup_function_exn "peducoml_divide") [ left; right ]
     | _ ->
       let result = build_binary_operation bop left right in
       ok result)
  | CApplication (callee, arg) ->
    let* callee, number_of_args = codegen_immexpr args_number env callee in
    let number_of_args =
      match params callee with
      | Some num -> num
      | None -> number_of_args
    in
    let func_ptr =
      build_call
        (lookup_function_exn "peducoml_alloc_closure")
        [ callee; const_int number_of_args ]
    in
    let* arg, _ = codegen_immexpr args_number env arg in
    let arg =
      match params arg with
      | None -> arg
      | Some num_args ->
        build_call
          (lookup_function_exn "peducoml_alloc_closure")
          [ arg; const_int num_args ]
    in
    ok @@ build_call (lookup_function_exn "peducoml_apply") [ func_ptr; arg ]
  | CUnaryOperation (op, arg) ->
    let* arg, _ = codegen_immexpr args_number env arg in
    let result = build_unary_operation op arg in
    ok result
  | CConstructList (head, tail) ->
    let* head, _ = codegen_immexpr args_number env head in
    let* tail, _ = codegen_immexpr args_number env tail in
    ok @@ build_call (lookup_function_exn "peducoml_add_to_list") [ tail; head ]
  | CIf (condition, true_branch, false_branch) ->
    let* condition, _ = codegen_immexpr args_number env condition in
    let alloca = build_alloca () in
    let false_label = get_basicblock "FB" in
    let joinup_label = get_basicblock "JB" in
    build_beq condition false_label;
    let* true_branch = codegen_aexpr args_number env true_branch in
    let true_branch =
      match params true_branch with
      | None -> true_branch
      | Some num_args ->
        build_call
          (lookup_function_exn "peducoml_alloc_closure")
          [ true_branch; const_int num_args ]
    in
    let _ = build_store_dst true_branch alloca in
    build_jump joinup_label;
    build_basicblock false_label;
    let* false_branch = codegen_aexpr args_number env false_branch in
    let false_branch =
      match params false_branch with
      | None -> false_branch
      | Some num_args ->
        build_call
          (lookup_function_exn "peducoml_alloc_closure")
          [ false_branch; const_int num_args ]
    in
    let _ = build_store_dst false_branch alloca in
    build_basicblock joinup_label;
    ok alloca

and codegen_aexpr args_number env = function
  | ACExpr cexpr -> codegen_cexpr args_number env cexpr
  | ALet (id, cexpr, aexpr) ->
    let* cexpr_rv_value = codegen_cexpr args_number env cexpr in
    let env = set env ~key:id ~data:cexpr_rv_value in
    codegen_aexpr args_number env aexpr
;;

let codegen_global_scope_function args_numbers (func : global_scope_function) =
  let function_name, arg_list, body = func in
  let func_rv_value, args = declare_function function_name arg_list in
  let env =
    Base.List.fold_right args ~init:empty ~f:(fun (arg, location) acc ->
      set acc ~key:arg ~data:location)
  in
  let* body = codegen_aexpr (Base.Map.find_exn args_numbers function_name) env body in
  let body =
    match type_of body with
    | Binding ->
      let num_args = params body |> Option.get in
      build_call
        (lookup_function_exn "peducoml_alloc_closure")
        [ body; const_int num_args ]
    | _ -> body
  in
  build_ret function_name body;
  ok func_rv_value
;;

let codegen program : (unit, riscv_error) Result.t =
  let args_number = gather_args_numbers program in
  let rec codegen = function
    | [] -> ok ()
    | head :: tail ->
      let* _ = codegen_global_scope_function args_number head in
      codegen tail
  in
  codegen program
;;
