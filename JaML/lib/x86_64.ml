(** Copyright 2024, Ilya Pankratov, Maxim Drumov *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open Anf
open Monads.CompilerMonad

let extern_function = [ "print_int"; "print_bool" ]

let op_to_assembly op l r =
  let open Ast in
  let* zero = make_integer 0 in
  let* zrax = mov_cmd R.rax zero in
  let rdx = reg_to_asmvalue R.rdx in
  let* mov_l = mov_cmd R.rax l in
  match op with
  | Add ->
    let* mov_r = mov_cmd R.rdx r in
    let* c = add_cmd R.rax rdx in
    return @@ mov_l @ mov_r @ c
  | Sub ->
    let* mov_r = mov_cmd R.rdx r in
    let* c = sub_cmd R.rax rdx in
    return @@ mov_l @ mov_r @ c
  | Div ->
    let* mov_r = mov_cmd R.rbx r in
    let* cqp = cqo_cmd ~comment:None in
    let* c = idiv_cmd (reg_to_asmvalue R.rbx) in
    return @@ mov_l @ mov_r @ (cqp :: c)
  | Mul ->
    let* mov_r = mov_cmd R.rdx r in
    let* c = imul_cmd R.rax rdx in
    return @@ mov_l @ mov_r @ c
  | Xor ->
    let* mov_r = mov_cmd R.rdx r in
    let* c = xor_cmd R.rax rdx in
    return @@ mov_l @ mov_r @ c
  | And ->
    let* mov_r = mov_cmd R.rdx r in
    let* c = and_cmd R.rax rdx in
    return @@ mov_l @ mov_r @ c
  | Or ->
    let* mov_r = mov_cmd R.rdx r in
    let* c = or_cmd R.rax rdx in
    return @@ mov_l @ mov_r @ c
  | Eq ->
    let* mov_r = mov_cmd R.rdx r in
    let* c1 = cmp_cmd R.rax rdx in
    let* c2 = sete_cmd R.rax in
    return (mov_l @ mov_r @ c1 @ zrax @ c2)
  | Neq ->
    let* mov_r = mov_cmd R.rdx r in
    let* c1 = cmp_cmd R.rax rdx in
    let* c2 = setne_cmd R.rax in
    return (mov_l @ mov_r @ c1 @ zrax @ c2)
  | Gt ->
    let* mov_r = mov_cmd R.rdx r in
    let* c1 = cmp_cmd R.rax rdx in
    let* c2 = setg_cmd R.rax in
    return (mov_l @ mov_r @ c1 @ zrax @ c2)
  | Lt ->
    let* mov_r = mov_cmd R.rdx r in
    let* c1 = cmp_cmd R.rax rdx in
    let* c2 = setl_cmd R.rax in
    return (mov_l @ mov_r @ c1 @ zrax @ c2)
  | Gte ->
    let* mov_r = mov_cmd R.rdx r in
    let* c1 = cmp_cmd R.rax rdx in
    let* c2 = setge_cmd R.rax in
    return (mov_l @ mov_r @ c1 @ zrax @ c2)
  | Lte ->
    let* mov_r = mov_cmd R.rdx r in
    let* c1 = cmp_cmd R.rax rdx in
    let* c2 = setle_cmd R.rax in
    return (mov_l @ mov_r @ c1 @ zrax @ c2)
;;

let assembly_immexpr = function
  | ImmNum num ->
    let* int = make_integer num in
    return (empty_cmd, int)
  | ImmBool boolean ->
    let* bool = make_bool boolean in
    return (empty_cmd, bool)
  | ImmId id -> get_data_from_environment id
;;

let assembly_args args =
  let* cmds, args =
    Base.List.fold
      ~init:(return ([], []))
      ~f:(fun acc el ->
        let* cmd, arg = assembly_immexpr el in
        let* cmds, args = acc in
        return (cmd :: cmds, arg :: args))
      args
  in
  return (List.rev cmds, List.rev args)
;;

let rec assembly_aexpr = function
  | ACEexpr cexpr -> assembly_cexpr cexpr
  | ALet (name, cexrp, aexpr) ->
    let* body_cmd = assembly_cexpr cexrp in
    let* save_cmd = save_return_value name in
    let* aexpr_cmds = assembly_aexpr aexpr in
    return @@ List.concat [ body_cmd; save_cmd :: aexpr_cmds ]

and assembly_cexpr = function
  | CBinOp (op, l, r) ->
    let* cmd1, l = assembly_immexpr l in
    let* cmd2, r = assembly_immexpr r in
    let* op_cmds = op_to_assembly op l r in
    return ([ cmd1; cmd2 ] @ op_cmds)
  | CApp (f, args) ->
    let* cmd, func_name = assembly_immexpr f in
    let* cmds1, func_args = assembly_args args in
    let* cmds2 = make_func_call func_name func_args in
    return ((cmd :: cmds1) @ cmds2)
  | CTuple elems ->
    let* cmds1, args = assembly_args elems in
    let* arg_num = make_integer (List.length args) in
    let* cmd, make_tuple = get_data_from_environment "tuple_make" in
    let* cmds2 = make_func_call make_tuple (arg_num :: args) in
    return (cmds1 @ (cmd :: cmds2))
  | CTake (tuple, n) ->
    let* cmd1, tuple = assembly_immexpr tuple in
    let* n = make_integer n in
    let* cmd2, take = get_data_from_environment "tuple_take" in
    let* cmds = make_func_call take [ tuple; n ] in
    return (cmd1 :: cmd2 :: cmds)
  | CMakeClosure (f, max_args, n, args) ->
    let* cmd1, f = assembly_immexpr f in
    let* max_args = make_integer max_args in
    let* n = make_integer n in
    let* cmds1, args = assembly_args args in
    let* cmd2, make_closure = get_data_from_environment "make_pa" in
    let* cmds2 = make_func_call make_closure (f :: max_args :: n :: args) in
    return ((cmd1 :: cmds1) @ (cmd2 :: cmds2))
  | CAddArgsToClosure (f, n, args) ->
    let* cmd0, f = assembly_immexpr f in
    let* n = make_integer n in
    let* cmds1, args = assembly_args args in
    let* cmd2, make_closure = get_data_from_environment "add_args_to_pa" in
    let* cmds3 = make_func_call make_closure (f :: n :: args) in
    return ((cmd0 :: cmds1) @ (cmd2 :: cmds3))
  | CImmExpr imm ->
    let* c1, imm = assembly_immexpr imm in
    let* c2 = mov_cmd R.rax imm in
    return (c1 :: c2)
  | CIfThenElse (i, t, e) ->
    let* else_if = fresh "else_if" in
    let* enf_if = fresh "end_if" in
    let* else_label = make_label else_if in
    let* end_label = make_label enf_if in
    let* _, if_value = assembly_immexpr i in
    let* cmd_mov = mov_cmd R.rax if_value in
    let* one = make_integer 1 in
    let* cmd_cmp = cmp_cmd R.rax one in
    let* cmd_jne = jne_cmd else_label in
    let* _ = start_branch in
    let* then_cmds = assembly_aexpr t in
    let* cmd_jmp = jmp_cmd end_label in
    let* _ = end_branch in
    let* _ = start_branch in
    let* else_cmds = assembly_aexpr e in
    let* _ = end_branch in
    let* else_label = make_label_cmd else_label in
    let* end_label = make_label_cmd end_label in
    return
      ((cmd_mov @ cmd_cmp @ (cmd_jne :: then_cmds) @ [ cmd_jmp ])
       @ (else_label :: else_cmds)
       @ [ end_label ])
;;

let assembly_anfexpr (AnfLetFun (name, args, body)) =
  let args =
    Base.List.map
      ~f:(function
        | Used s -> s
        | _ -> "_unused")
      args
  in
  let* cmds1 = make_func_preambula name args in
  let* cmds2 = assembly_aexpr body in
  let* cmds3 = make_func_ret in
  return @@ List.concat [ cmds1; cmds2; cmds3 ]
;;

let assembly l =
  let runtime_functions = [ "make_pa"; "add_args_to_pa"; "tuple_make"; "tuple_take" ] in
  let* runtime =
    monad_map
      ~f:(fun f ->
        let* label = make_label f in
        add_extern label)
      runtime_functions
  in
  let* externs =
    monad_map
      ~f:(fun (f, _, _) ->
        let* label = make_label f in
        add_extern label)
      Jamlstdlib.stdlib_functions
  in
  let* main_label = make_label "main" in
  let* global = add_global main_label in
  let* functions = monad_map l ~f:assembly_anfexpr in
  return @@ runtime @ externs @ (global :: List.concat functions)
;;

let assembly l = run (assembly l)
