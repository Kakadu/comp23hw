(** Copyright 2024, Ilya Pankratov, Maxim Drumov *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open Llvm
open Anf
open Base

let ctx = create_context ()
let md = create_module ctx "Jaml"
let builder = builder ctx
let i64 = i64_type ctx
let func_ty = function_type i64 [| i64 |]
let func2_ty = function_type i64 [| i64; i64 |]
let va_func_ty arr = var_arg_function_type i64 arr
let lookup_function_exn name = Stdlib.Option.get @@ lookup_function name md
let named_values = Hashtbl.create (module String)
let rev lst = Array.rev @@ Array.of_list lst

let rec codegen_imm_args args =
  rev @@ List.fold ~f:(fun acc arg -> codegen_imm arg :: acc) ~init:[] args

and codegen_imm = function
  | ImmNum i -> const_int i64 i
  | ImmBool b -> const_int i64 (Bool.to_int b)
  | ImmId id ->
    (match lookup_function id md with
     | Some v -> v
     | None ->
       build_load i64 (Stdlib.Option.get @@ Hashtbl.find named_values id) id builder)
;;

let bin_op op l' r' =
  let open Ast in
  let f =
    match op with
    | Add | Sub | Div | Mul -> fun x -> x
    | _ -> fun llvalue -> build_zext llvalue i64 "to_int" builder
  in
  let op =
    match op with
    | Add -> build_add l' r' "add" builder
    | Sub -> build_sub l' r' "sub" builder
    | Div -> build_sdiv l' r' "div" builder
    | Mul -> build_mul l' r' "mul" builder
    | Xor -> build_xor l' r' "xor" builder
    | And -> build_and l' r' "and" builder
    | Or -> build_or l' r' "or" builder
    | Eq -> build_icmp Icmp.Eq l' r' "eq" builder
    | Neq -> build_icmp Icmp.Ne l' r' "neq" builder
    | Gt -> build_icmp Icmp.Sgt l' r' "gt" builder
    | Lt -> build_icmp Icmp.Slt l' r' "lt" builder
    | Gte -> build_icmp Icmp.Sge l' r' "gte" builder
    | Lte -> build_icmp Icmp.Sle l' r' "lte" builder
  in
  f op
;;

let rec codegen_cexpr = function
  | CBinOp (s, l, r) ->
    let l' = codegen_imm l in
    let r' = codegen_imm r in
    bin_op s l' r'
  | CImmExpr imm -> codegen_imm imm
  | CApp (callee, args) ->
    let callee = codegen_imm callee in
    let args, args_types =
      List.fold
        ~f:(fun (args, types) arg -> codegen_imm arg :: args, i64 :: types)
        ~init:([], [])
        args
    in
    let fnty = function_type i64 (rev args_types) in
    build_call fnty callee (rev args) "apply_n" builder
  | CMakeClosure (callee, max_args, app_args, args) ->
    let callee = codegen_imm callee in
    let args = codegen_imm_args args in
    build_call
      func2_ty
      (lookup_function_exn "make_pa")
      (Array.append
         [| build_pointercast callee i64 "pointer_to_int" builder
          ; const_int i64 max_args
          ; const_int i64 app_args
         |]
         args)
      "make_pa"
      builder
  | CAddArgsToClosure (callee, app_args, args) ->
    let callee = codegen_imm callee in
    let args = codegen_imm_args args in
    build_call
      func2_ty
      (lookup_function_exn "add_args_to_pa")
      (Array.append [| callee; const_int i64 app_args |] args)
      "add_args_to_pa"
      builder
  | CTuple immexpr ->
    let immlst = codegen_imm_args immexpr in
    build_call
      func_ty
      (lookup_function_exn "tuple_make")
      (Array.append [| const_int i64 @@ Array.length immlst |] immlst)
      "tuple_make"
      builder
  | CTake (immexpr, index) ->
    let immexpr = codegen_imm immexpr in
    build_call
      func2_ty
      (lookup_function_exn "tuple_take")
      [| immexpr; const_int i64 index |]
      "tuple_take"
      builder
  | CIfThenElse (cond, th, el) ->
    let condition = codegen_imm cond in
    let condition = condition in
    let zero = const_int i64 0 in
    let cond_val = build_icmp Icmp.Ne condition zero "if_cond" builder in
    let start_bb = insertion_block builder in
    let func = block_parent start_bb in
    let then_bb = append_block ctx "then" func in
    position_at_end then_bb builder;
    let then_val = codegen_aexpr th in
    let new_then_bb = insertion_block builder in
    let else_bb = append_block ctx "else" func in
    position_at_end else_bb builder;
    let else_val = codegen_aexpr el in
    let new_else_bb = insertion_block builder in
    let merge_bb = append_block ctx "if_ctx" func in
    position_at_end merge_bb builder;
    let incoming = [ then_val, new_then_bb; else_val, new_else_bb ] in
    let phi = build_phi incoming "if_phi" builder in
    position_at_end start_bb builder;
    let (_ : Llvm.llvalue) = build_cond_br cond_val then_bb else_bb builder in
    position_at_end new_then_bb builder;
    let (_ : Llvm.llvalue) = build_br merge_bb builder in
    position_at_end new_else_bb builder;
    let (_ : Llvm.llvalue) = build_br merge_bb builder in
    position_at_end merge_bb builder;
    phi

and codegen_aexpr = function
  | ACEexpr cexpr -> codegen_cexpr cexpr
  | ALet (name, cexpr, aexpr) ->
    let alloca = build_alloca i64 name builder in
    let cexpr = codegen_cexpr cexpr in
    let (_ : Llvm.llvalue) = build_store cexpr alloca builder in
    Hashtbl.set named_values ~key:name ~data:alloca;
    codegen_aexpr aexpr
;;

let codegen_anfexpr (AnfLetFun (name, args, aexpr)) =
  let arg_arr = List.map ~f:(Fn.const i64) args in
  let func2_type = function_type i64 (Array.of_list arg_arr) in
  let func_val = declare_function name func2_type md in
  let entry = append_block ctx "entry" func_val in
  position_at_end entry builder;
  Array.iteri
    ~f:(fun i arg ->
      match List.nth_exn args i with
      | Used name' ->
        let alloca = build_alloca i64 name' builder in
        let (_ : Llvm.llvalue) = build_store arg alloca builder in
        set_value_name name' arg;
        Hashtbl.set named_values ~key:name' ~data:alloca
      | Unused -> ())
    (params func_val);
  let aexpr = codegen_aexpr aexpr in
  let (_ : Llvm.llvalue) = build_ret aexpr builder in
  func_val
;;

let compile f =
  let runtime =
    [ declare_function "tuple_make" (va_func_ty [| i64 |]) md
    ; declare_function "tuple_take" func2_ty md
    ; declare_function "make_pa" (va_func_ty [| i64; i64; i64 |]) md
    ; declare_function "add_args_to_pa" (va_func_ty [| i64; i64 |]) md
    ; declare_function "print_int" func_ty md
    ; declare_function "print_bool" func_ty md
    ]
  in
  List.rev @@ List.fold ~f:(fun acc func -> codegen_anfexpr func :: acc) ~init:runtime f
;;
