(** Copyright 2023-2024, Anton Kraev and Polina Badreeva *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Llvm
open Anf
open Ast

let context = global_context ()
let the_module = create_module context "main"
let builder = builder context
let i32_t = i32_type context
let named_values = Hashtbl.create 100

let get_value name =
  try Hashtbl.find named_values name with
  | Not_found -> failwith ("unbound value " ^ name)
;;

let immexpr_to_llvm_ir = function
  | ImmInt i -> const_int i32_t i
  | ImmBool b -> const_int i32_t (if b then 1 else 0)
  | ImmIdentifier name -> get_value name
;;

let cexpr_to_llvm_ir = function
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
    build_call limm_val [| rimm_val |] "call" builder
  | CIfThenElse (cond, t_branch, f_branch) ->
    let cond_val = immexpr_to_llvm_ir cond in
    let start_block = insertion_block builder in
    let the_function = block_parent start_block in
    let true_block = append_block context "then" the_function in
    let false_block = append_block context "else" the_function in
    let merge_block = append_block context "ifcont" the_function in
    ignore (build_cond_br cond_val true_block false_block builder);
    position_at_end true_block builder;
    let true_val = immexpr_to_llvm_ir t_branch in
    ignore (build_br merge_block builder);
    let true_block = insertion_block builder in
    position_at_end false_block builder;
    let false_val = immexpr_to_llvm_ir f_branch in
    ignore (build_br merge_block builder);
    let false_block = insertion_block builder in
    position_at_end merge_block builder;
    build_phi [ true_val, true_block; false_val, false_block ] "iftmp" builder
;;

let rec aexpr_to_llvm_ir = function
  | ALet (name, left, right) ->
    let av = cexpr_to_llvm_ir left in
    Hashtbl.add named_values name av;
    aexpr_to_llvm_ir right
  | ACExpr expr -> cexpr_to_llvm_ir expr
;;

let abinding_to_llvm_ir = function
  | AVal (name, aexpr) ->
    (match aexpr with
     | ACExpr (CImmExpr (ImmIdentifier n)) ->
       let n_function = Hashtbl.find named_values n in
       let return_type = element_type (return_type (type_of n_function)) in
       let ft =
         function_type
           return_type
           (Array.map (fun param -> type_of param) (params n_function))
       in
       let the_function = declare_function name ft the_module in
       let bb = append_block context "entry" the_function in
       position_at_end bb builder;
       let params = params the_function in
       let call_inst = build_call n_function params "call" builder in
       ignore (build_ret call_inst builder);
       the_function
     | _ ->
       let aexpr_val = aexpr_to_llvm_ir aexpr in
       Hashtbl.add named_values name aexpr_val;
       aexpr_val)
       | AFun (func_name, arg_names, body) ->
        let arg_types = Array.make (List.length arg_names) i32_t in
        let ft = function_type i32_t arg_types in
        let func_val = declare_function func_name ft the_module in
        Hashtbl.add named_values func_name func_val;
        let bb = append_block context "entry" func_val in
        position_at_end bb builder;
        Array.iteri
          (fun i arg ->
            let name = List.nth arg_names i in
            set_value_name name arg;
            Hashtbl.add named_values name arg)
          (params func_val);
      
        let ret_val = aexpr_to_llvm_ir body in
        let ret_val_conv = build_zext ret_val i32_t "boolToInt" builder in  (* convert return value from i1 to i32 *)
      
        ignore (build_ret ret_val_conv builder);  (* Using converted value here *)
        func_val
;;

let llvm_program program = List.map abinding_to_llvm_ir program
