(** Copyright 2023-2024, Danila Pechenev, Ilya Dudnikov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Anf
open Arg_counter
open Ast
open Llvm

let context = global_context ()
let builder = builder context
let the_module = create_module context "PeDuCoML"
let i64 = i64_type context
let lookup_function_exn id llmodule = Option.get @@ lookup_function id llmodule

type llvm_error = UnboundVariable of unique_id

(* Smart constructors *)
let unbound id = UnboundVariable id
(* ------------------ *)

open Result

let ( let* ) = bind

let string_of_unique_id = function
  | AnfId id -> string_of_int id
  | GlobalScopeId id -> id
;;

let rec codegen_immexpr args_numbers env =
  let list_helper imm_list =
    let allocated_list =
      build_call
        (function_type i64 [||])
        (lookup_function_exn "peducoml_alloc_list" the_module)
        [||]
        "peducoml_alloc_list_n"
        builder
    in
    let add = lookup_function_exn "peducoml_add_to_list" the_module in
    let fnty = function_type i64 [| i64; i64 |] in
    ( Base.List.fold (Base.List.rev imm_list) ~init:(ok allocated_list) ~f:(fun acc elem ->
        let* elem = codegen_immexpr args_numbers env elem |> fst in
        let* acc = acc in
        ok @@ build_call fnty add [| acc; elem |] "peducoml_add_to_list_n" builder)
    , (0, false) )
  in
  function
  (* Returns: (llvalue, (number of args of function-argument, whether function is global scope one)) *)
  | ImmInt num -> ok @@ const_int i64 num, (0, false)
  | ImmString str ->
    Base.List.init (Base.String.length str) ~f:(Base.String.get str)
    |> Base.List.map ~f:(fun x -> ImmChar x)
    |> list_helper
  | ImmChar c -> ok @@ const_int i64 (Base.Char.to_int c), (0, false)
  | ImmBool v -> ok @@ const_int i64 (Base.Bool.to_int v), (0, false)
  | ImmId unq_id ->
    (match Base.Map.Poly.find env unq_id, unq_id with
     | _, GlobalScopeId id ->
       let llv = lookup_function_exn id the_module in
       if params llv |> Base.Array.length = 0
       then
         ( ok
           @@ build_call
                (function_type i64 [| i64 |])
                (lookup_function_exn "peducoml_apply0" the_module)
                [| build_pointercast llv i64 "ptr_to_i64_n" builder |]
                "peducoml_apply0_n"
                builder
         , (0, false) )
       else ok llv, (0, true)
     | None, _ -> error @@ unbound unq_id, (0, false)
     | Some llvalue, AnfId id ->
       let number_of_args =
         match Base.Map.Poly.find args_numbers unq_id with
         | Some n -> n
         | None -> 0
       in
       ok @@ build_load i64 llvalue (string_of_int id) builder, (number_of_args, false))
  | ImmList imm_list -> list_helper imm_list
  | ImmTuple imm_list ->
    let allocated_tuple =
      build_call
        (function_type i64 [| i64 |])
        (lookup_function_exn "peducoml_alloc_tuple" the_module)
        [| Base.List.length imm_list |> const_int i64 |]
        "peducoml_alloc_tuple_n"
        builder
    in
    ( Base.List.fold imm_list ~init:(ok allocated_tuple) ~f:(fun acc imm ->
        let* elem = codegen_immexpr args_numbers env imm |> fst in
        let* acc = acc in
        ok
        @@ build_call
             (function_type i64 [| i64; i64 |])
             (lookup_function_exn "peducoml_fill_tuple" the_module)
             [| acc; elem |]
             "peducoml_fill_tuple_n"
             builder)
    , (0, false) )
;;

let build_binary_operation = function
  | Add -> build_add
  | Sub -> build_sub
  | Mul -> build_mul
  | Div -> build_udiv
  | Eq -> build_icmp Icmp.Eq
  | NEq -> build_icmp Icmp.Ne
  | GT -> build_icmp Icmp.Sgt
  | GTE -> build_icmp Icmp.Sge
  | LT -> build_icmp Icmp.Slt
  | LTE -> build_icmp Icmp.Sle
  | AND -> build_and
  | OR -> build_or
;;

let build_unary_operation = function
  | Minus -> build_neg
  | Not -> build_not
;;

let rec codegen_cexpr args_numbers env = function
  | CImm imm_expr -> fst @@ codegen_immexpr args_numbers env imm_expr
  | CBinaryOperation (bop, left_imm, right_imm) ->
    let* left = fst @@ codegen_immexpr args_numbers env left_imm in
    let* right = fst @@ codegen_immexpr args_numbers env right_imm in
    (match bop with
     | Div ->
       ok
       @@ build_call
            (function_type i64 [| i64; i64 |])
            (lookup_function_exn "peducoml_divide" the_module)
            [| left; right |]
            "peducoml_divide_n"
            builder
     | _ ->
       let result =
         build_binary_operation
           bop
           left
           right
           (show_binary_operator bop ^ "result_n")
           builder
       in
       let result = build_zext result i64 "zext_n" builder in
       ok result)
  | CUnaryOperation (uop, arg) ->
    let* arg = fst @@ codegen_immexpr args_numbers env arg in
    let result =
      build_unary_operation uop arg (show_unary_operator uop ^ "result_n") builder
    in
    let result = build_zext result i64 "zext_n" builder in
    ok result
  | CApplication (func, arg) ->
    let callee, (number_of_args, is_global) = codegen_immexpr args_numbers env func in
    let* callee = callee in
    let number_of_args =
      if is_global then params callee |> Base.Array.length else number_of_args
    in
    let arg, (arg_nargs, arg_is_global) = codegen_immexpr args_numbers env arg in
    let* arg = arg in
    let arg =
      if type_of arg = pointer_type context
      then
        build_call
          (function_type i64 [| i64; i64 |])
          (lookup_function_exn "peducoml_alloc_closure" the_module)
          [| build_pointercast arg i64 "ptr_to_i64_n" builder
           ; const_int
               i64
               (if arg_is_global then params arg |> Base.Array.length else arg_nargs)
          |]
          "peducoml_alloc_closure_n"
          builder
      else arg
    in
    let func_ptr =
      build_call
        (function_type i64 [| i64; i64 |])
        (lookup_function_exn "peducoml_alloc_closure" the_module)
        [| build_pointercast callee i64 "ptr_to_i64_n" builder
         ; const_int i64 number_of_args
        |]
        "peducoml_alloc_closure_n"
        builder
    in
    ok
    @@ build_call
         (function_type i64 [| i64; i64 |])
         (lookup_function_exn "peducoml_apply" the_module)
         [| func_ptr; arg |]
         "peducoml_apply_n"
         builder
  | CIf (condition, then_branch, else_branch) ->
    let condition, _ = codegen_immexpr args_numbers env condition in
    let* condition = condition in
    let zero = const_int i64 0 in
    let condition_value = build_icmp Icmp.Ne condition zero "condition_value_n" builder in
    let start_bb = insertion_block builder in
    let func = block_parent start_bb in
    let then_bb = append_block context "then_branch_n" func in
    position_at_end then_bb builder;
    let then_branch = codegen_aexpr args_numbers env then_branch in
    let* then_branch = then_branch in
    let new_then_bb = insertion_block builder in
    let else_bb = append_block context "else_branch_n" func in
    position_at_end else_bb builder;
    let else_branch = codegen_aexpr args_numbers env else_branch in
    let* else_branch = else_branch in
    let new_else_bb = insertion_block builder in
    let merge_bb = append_block context "if_context_n" func in
    position_at_end merge_bb builder;
    let incoming = [ then_branch, new_then_bb; else_branch, new_else_bb ] in
    let phi = build_phi incoming "if_phi_n" builder in
    position_at_end start_bb builder;
    let (_ : llvalue) = build_cond_br condition_value then_bb else_bb builder in
    position_at_end new_then_bb builder;
    let (_ : llvalue) = build_br merge_bb builder in
    position_at_end new_else_bb builder;
    let (_ : llvalue) = build_br merge_bb builder in
    position_at_end merge_bb builder;
    ok phi
  | CConstructList (arg_value, arg_list) ->
    let arg_value, _ = codegen_immexpr args_numbers env arg_value in
    let* arg_value = arg_value in
    let arg_list, _ = codegen_immexpr args_numbers env arg_list in
    let* arg_list = arg_list in
    let add_to_list = lookup_function_exn "peducoml_add_to_list" the_module in
    let fnty = function_type i64 [| i64; i64 |] in
    let result =
      build_call
        fnty
        add_to_list
        [| arg_list; arg_value |]
        "peducoml_add_to_list_n"
        builder
    in
    ok result

and codegen_aexpr args_numbers env = function
  | ACExpr cexpr -> codegen_cexpr args_numbers env cexpr
  | ALet (id, cexpr, aexpr) ->
    let alloca = build_alloca i64 (string_of_unique_id id) builder in
    let* cexpr = codegen_cexpr args_numbers env cexpr in
    let (_ : llvalue) = build_store cexpr alloca builder in
    let env = Base.Map.Poly.set env ~key:id ~data:alloca in
    codegen_aexpr args_numbers env aexpr
;;

let codegen_global_scope_function args_numbers env (func : global_scope_function) =
  let function_name, arg_list, body = func in
  let fnty = function_type i64 (Array.make (Base.List.length arg_list) i64) in
  let func = declare_function function_name fnty the_module in
  Base.Array.iter
    (Base.Array.zip_exn (Base.List.to_array arg_list) (params func))
    ~f:(fun (name, value) -> set_value_name (string_of_unique_id name) value);
  let basic_block = append_block context "entry" func in
  position_at_end basic_block builder;
  let env_with_args =
    Base.Array.fold_right
      (Base.Array.zip_exn (Base.List.to_array arg_list) (params func))
      ~init:env
      ~f:(fun (name, value) acc ->
        let alloca = build_alloca i64 (string_of_unique_id name) builder in
        let (_ : llvalue) = build_store value alloca builder in
        Base.Map.Poly.set acc ~key:name ~data:alloca)
  in
  let* return_val =
    codegen_aexpr (Base.Map.find_exn args_numbers function_name) env_with_args body
  in
  let return_val =
    if type_of return_val = pointer_type context
    then
      build_call
        (function_type i64 [| i64; i64 |])
        (lookup_function_exn "peducoml_alloc_closure" the_module)
        [| build_pointercast return_val i64 "ptr_to_i64_n" builder
         ; params return_val |> Base.Array.length |> const_int i64
        |]
        "peducoml_alloc_closure_n"
        builder
    else return_val
  in
  let _ = build_ret return_val builder in
  ok (func, env)
;;

open Peducoml_stdlib
open Peducoml_runtime
open Typing

let codegen program =
  let rec count_args current = function
    | TArr (_, out_ty) -> count_args (current + 1) out_ty
    | _ -> current
  in
  let env =
    Base.List.map (runtime @ stdlib) ~f:(fun (id, fun_type) ->
      declare_function
        id
        (function_type i64 (Array.make (count_args 0 @@ snd fun_type) i64))
        the_module)
  in
  let args_numbers = gather_args_numbers program in
  let rec codegen acc env = function
    | [] -> ok acc
    | head :: tail ->
      let* head, env = codegen_global_scope_function args_numbers env head in
      codegen (head :: acc) env tail
  in
  let* result = codegen env Base.Map.Poly.empty program in
  ok @@ Base.List.rev result
;;
