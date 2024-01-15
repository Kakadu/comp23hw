(** Copyright 2023-2024, Anton Kraev and Polina Badreeva *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Llvm
open Anf
open Ast

let context = global_context ()
let the_module = create_module context "main"
let builder = builder context
let i64_t = i64_type context
let runtime_fun_t = function_type i64_t [| i64_t; i64_t |]
let lookup_function_exn id = Option.get @@ lookup_function id the_module
let named_values = Hashtbl.create 32

let immexpr_to_llvm_ir = function
  | ImmInt i -> const_int i64_t i
  | ImmBool b -> const_int i64_t @@ Bool.to_int b
  | ImmIdentifier name ->
    (match Hashtbl.find_opt named_values name with
     | Some v -> build_load i64_t v name builder
     | None ->
       let v = lookup_function_exn name in
       build_call
         runtime_fun_t
         (lookup_function_exn "alloc_closure")
         [| build_pointercast v i64_t "ptr_to_int" builder
          ; const_int i64_t @@ Array.length @@ params v
         |]
         "alloc_closure"
         builder)
;;

let rec cexpr_to_llvm_ir = function
  | CImmExpr imm -> immexpr_to_llvm_ir imm
  | CUnaryOp (op, imm) ->
    let imm_val = immexpr_to_llvm_ir imm in
    let unop =
      match op with
      | Not -> build_not imm_val "not" builder
      | Neg -> build_neg imm_val "neg" builder
    in
    build_zext unop i64_t "bool_to_int" builder
  | CBinaryOp (op, limm, rimm) ->
    let limm_val = immexpr_to_llvm_ir limm in
    let rimm_val = immexpr_to_llvm_ir rimm in
    let binop =
      match op with
      | Add -> build_add limm_val rimm_val "add" builder
      | Sub -> build_sub limm_val rimm_val "sub" builder
      | Mult -> build_mul limm_val rimm_val "mult" builder
      | Div -> build_sdiv limm_val rimm_val "div" builder
      | And -> build_and limm_val rimm_val "and" builder
      | Or -> build_or limm_val rimm_val "or" builder
      | Eq -> build_icmp Icmp.Eq limm_val rimm_val "eq" builder
      | Lt -> build_icmp Icmp.Ult limm_val rimm_val "lt" builder
      | LtOrEq -> build_icmp Icmp.Ule limm_val rimm_val "lte" builder
      | Gt -> build_icmp Icmp.Ugt limm_val rimm_val "qt" builder
      | GtOrEq -> build_icmp Icmp.Uge limm_val rimm_val "qte" builder
    in
    build_zext binop i64_t "bool_to_int" builder
  | CApp (limm, rimm) ->
    let limm_val = immexpr_to_llvm_ir limm in
    let rimm_val = immexpr_to_llvm_ir rimm in
    build_call
      runtime_fun_t
      (lookup_function_exn "apply_closure")
      [| limm_val; rimm_val |]
      "apply_closure"
      builder
  | CIfThenElse (cond, t_branch, f_branch) ->
    let start_block = block_parent @@ insertion_block builder in
    let append name = append_block context name start_block in
    let cond_block = append "cond" in
    let then_block = append "then" in
    let else_block = append "else" in
    let cond_val =
      build_icmp
        Icmp.Ne
        (immexpr_to_llvm_ir cond)
        (const_int i64_t 0)
        "int_to_bool"
        builder
    in
    let (_ : Llvm.llvalue) = build_cond_br cond_val then_block else_block builder in
    position_at_end then_block builder;
    let then_val = aexpr_to_llvm_ir t_branch in
    let (_ : Llvm.llvalue) = build_br cond_block builder in
    let then_block = insertion_block builder in
    position_at_end else_block builder;
    let else_val = aexpr_to_llvm_ir f_branch in
    let (_ : Llvm.llvalue) = build_br cond_block builder in
    let else_block = insertion_block builder in
    position_at_end cond_block builder;
    build_phi [ then_val, then_block; else_val, else_block ] "ite" builder

and aexpr_to_llvm_ir = function
  | ALet (name, left, right) ->
    let alloca = build_alloca i64_t name builder in
    let (_ : Llvm.llvalue) = build_store (cexpr_to_llvm_ir left) alloca builder in
    Hashtbl.add named_values name alloca;
    aexpr_to_llvm_ir right
  | ACExpr expr -> cexpr_to_llvm_ir expr
;;

let abinding_to_llvm_ir = function
  | AFun (func_name, arg_names, body) ->
    Hashtbl.clear named_values;
    let func_type = function_type i64_t (Array.make (List.length arg_names) i64_t) in
    let func_val = declare_function func_name func_type the_module in
    let entry = append_block context "entry" func_val in
    position_at_end entry builder;
    Array.iteri
      (fun i arg ->
        let name = List.nth arg_names i in
        let alloca = build_alloca i64_t name builder in
        let (_ : Llvm.llvalue) = build_store arg alloca builder in
        set_value_name name arg;
        Hashtbl.add named_values name alloca)
      (params func_val);
    let (_ : Llvm.llvalue) = build_ret (aexpr_to_llvm_ir body) builder in
    func_val
;;

let llvm_program program =
  let runtime =
    [ declare_function "alloc_closure" runtime_fun_t the_module
    ; declare_function "apply_closure" runtime_fun_t the_module
    ; declare_function "print_int" (function_type i64_t [| i64_t |]) the_module
    ; declare_function "print_bool" (function_type i64_t [| i64_t |]) the_module
    ]
  in
  List.rev
  @@ List.fold_left
       (fun processed ab -> abinding_to_llvm_ir ab :: processed)
       runtime
       program
;;
