(** Copyright 2023-2024, Anton Kraev and Polina Badreeva *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Llvm
open Anf
open Ast

let context = global_context ()
let the_module = create_module context "main"
let builder = builder context
let i64_t = i64_type context
let named_values = Hashtbl.create 100
let lookup_function_exn id = Option.get (lookup_function id the_module)

let immexpr_to_llvm_ir = function
  | ImmInt i -> const_int i64_t i
  | ImmBool b -> const_int i64_t (if b then 1 else 0)
  | ImmIdentifier name ->
    (match Hashtbl.find_opt named_values name with
     | Some v -> build_load i64_t v name builder
     | None ->
       let v = lookup_function_exn name in
       build_call
         (function_type i64_t [| i64_t; i64_t |])
         (lookup_function_exn "alloc_closure")
         [| build_pointercast v i64_t "cast_pointer_to_int" builder
          ; params v |> Array.length |> const_int i64_t
         |]
         "alloc_closure"
         builder)
;;

let rec cexpr_to_llvm_ir = function
  | CImmExpr imm -> immexpr_to_llvm_ir imm
  | CUnaryOp (op, imm) ->
    let imm_val = immexpr_to_llvm_ir imm in
    (match op with
     | Not -> build_not imm_val "not" builder
     | Neg -> build_neg imm_val "neg" builder)
  | CBinaryOp (op, limm, rimm) ->
    let limm_val = immexpr_to_llvm_ir limm in
    let rimm_val = immexpr_to_llvm_ir rimm in
    (match op with
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
     | GtOrEq -> build_icmp Icmp.Uge limm_val rimm_val "qte" builder)
  | CApp (limm, rimm) ->
    let limm_val = immexpr_to_llvm_ir limm in
    let rimm_val = immexpr_to_llvm_ir rimm in
    build_call
      i64_t
      (lookup_function_exn "apply_closure")
      [| limm_val; rimm_val |]
      "apply_closure"
      builder
  | CIfThenElse (cond, t_branch, f_branch) ->
    let cond_val = immexpr_to_llvm_ir cond in
    let start_block = insertion_block builder in
    let the_function = block_parent start_block in
    let true_block = append_block context "then" the_function in
    let false_block = append_block context "else" the_function in
    let merge_block = append_block context "ifcont" the_function in
    ignore (build_cond_br cond_val true_block false_block builder);
    position_at_end true_block builder;
    let true_val = aexpr_to_llvm_ir t_branch in
    ignore (build_br merge_block builder);
    let true_block = insertion_block builder in
    position_at_end false_block builder;
    let false_val = aexpr_to_llvm_ir f_branch in
    ignore (build_br merge_block builder);
    let false_block = insertion_block builder in
    position_at_end merge_block builder;
    build_phi [ true_val, true_block; false_val, false_block ] "iftmp" builder

and aexpr_to_llvm_ir = function
  | ALet (name, left, right) ->
    let av = cexpr_to_llvm_ir left in
    let alloca = build_alloca i64_t name builder in
    let (_ : Llvm.llvalue) = build_store av alloca builder in
    Hashtbl.add named_values name alloca;
    aexpr_to_llvm_ir right
  | ACExpr expr -> cexpr_to_llvm_ir expr
;;

let abinding_to_llvm_ir = function
  | AVal (name, aexpr) ->
    (match aexpr with
     | _ ->
       let aexpr_val = aexpr_to_llvm_ir aexpr in
       aexpr_val)
  | AFun (func_name, arg_names, body) ->
    Hashtbl.clear named_values;
    let arg_types = Array.make (List.length arg_names) i64_t in
    let ft = function_type i64_t arg_types in
    let func_val = declare_function func_name ft the_module in
    let bb = append_block context "entry" func_val in
    position_at_end bb builder;
    Array.iteri
      (fun i arg ->
        let name = List.nth arg_names i in
        set_value_name name arg)
      (params func_val);
    Array.iteri
      (fun i a ->
        let name = List.nth arg_names i in
        let alloca = build_alloca i64_t name builder in
        let (_ : Llvm.llvalue) = build_store a alloca builder in
        Hashtbl.add named_values name alloca)
      (params func_val);
    let ret_val = aexpr_to_llvm_ir body in
    ignore (build_ret ret_val builder);
    func_val
;;

let declare_functions () =
  ignore
    (declare_function "apply_closure" (function_type i64_t [| i64_t; i64_t |]) the_module);
  ignore
    (declare_function "alloc_closure" (function_type i64_t [| i64_t; i64_t |]) the_module)
;;

let llvm_program program =
  declare_functions ();
  List.map abinding_to_llvm_ir program
;;
