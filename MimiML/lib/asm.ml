(** Copyright 2023, Lev Golofastov & Ksenia Kuzmina *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Anf
include Format
include List

let generated = Buffer.create 100
let append code = Buffer.add_string generated @@ sprintf "%s\n" code

module SMap = Map.Make (String)

type loc =
  | StackLocation of int
  | Register of string
  | IntConstant of int
  | Displacement of string * int
  | SubDefenition of string

module AsmMonad = struct
  type context =
    { variable_bindings : loc SMap.t
    ; function_defenitions : string list
    ; function_arities : int SMap.t
    ; rsp_offset : int
    ; counter : int
    }

  type 'a t = context -> 'a * context

  let bind t f context =
    let a, context = t context in
    let b, context = f a context in
    b, context
  ;;

  let ( >>= ) = bind
  let ( let* ) = ( >>= )
  let return a context = a, context
  let variables context = context.variable_bindings, context
  let fundefs context = context.function_defenitions, context
  let arities context = context.function_arities, context
  let rsp_offset context = context.rsp_offset, context
  let next_cnt context = context.counter, { context with counter = context.counter + 1 }
  let add_to_rsp x context = (), { context with rsp_offset = context.rsp_offset + x }
  let sub_to_rsp x context = (), { context with rsp_offset = context.rsp_offset - x }
end

module AsmBuilderTools = struct
  let reg name = Register name
  let intconstant i = IntConstant i
  let displacement reg d = Displacement (reg, d)
  let subdef name = SubDefenition name

  let loc_to_string = function
    | StackLocation d when d > 0 -> Format.sprintf "qword [rbp+%d]" d
    | StackLocation d when d < 0 -> Format.sprintf "qword [rbp%d]" d
    | StackLocation _ -> Format.sprintf "qword [rbp]"
    | Register r -> r
    | IntConstant c -> Format.sprintf "%+d" c
    | Displacement (r, d) -> Format.sprintf "qword [%s%+d]" r d
    | SubDefenition fname -> fname
  ;;

  let rax = reg "rax"
  let rbx = reg "rbx"
  let rcx = reg "rcx"
  let rdx = reg "rdx"
  let rci = reg "rci"
  let rdi = reg "rdi"
  let rsi = reg "rsi"
  let rsp = reg "rsp"
  let rbp = reg "rbp"
  let r8 = reg "r8"
  let r9 = reg "r9"
  let r10 = reg "r10"
  let r11 = reg "r11"
  let r12 = reg "r12"
  let r13 = reg "r13"
  let r14 = reg "r14"
  let r15 = reg "r15"
  let unwrap1 f x = append @@ f @@ loc_to_string x
  let unwrap2 f x y = append @@ f (loc_to_string x) (loc_to_string y)
  let pop = unwrap1 @@ sprintf "  pop %s"
  let push = unwrap1 @@ sprintf "  push %s"
  let mov = unwrap2 @@ sprintf "  mov %s, %s"
  let movzx = unwrap2 @@ sprintf "  movzx %s, %s"
  let add = unwrap2 @@ sprintf "  add %s, %s"
  let sub = unwrap2 @@ sprintf "  sub %s, %s"
  let imul = unwrap2 @@ sprintf "  imul %s, %s"
  let idiv = unwrap1 @@ sprintf "  idiv %s"
  let ret () = append "  ret"
  let setle = unwrap1 @@ sprintf "  setle %s"
  let setl = unwrap1 @@ sprintf "  setl %s"
  let setge = unwrap1 @@ sprintf "  setge %s"
  let setg = unwrap1 @@ sprintf "  setg %s"
  let sete = unwrap1 @@ sprintf "  sete %s"
  let cmp = unwrap2 @@ sprintf "  cmp %s, %s"
  let call = unwrap1 @@ sprintf "  call %s"
  let je = unwrap1 @@ sprintf "  je %s"
  let jmp = unwrap1 @@ sprintf "  jmp %s"
  let label = unwrap1 @@ sprintf " %s:"
  let comment s = append @@ sprintf "  ; %s" s
end

include AsmMonad
include AsmBuilderTools

let explode s =
  let rec exp i l = if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) []
;;

let is_alphanumeric s =
  let is_alphanumeric_inner = function
    | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '.' -> true
    | _ -> false
  in
  String.for_all is_alphanumeric_inner s
;;

let allocate_closure function_name arity =
  let function_pointer = subdef function_name in
  mov rdi (intconstant @@ (arity * 8));
  call (subdef "m_malloc");
  mov rdx rax;
  mov rdi function_pointer;
  mov rsi (intconstant 0);
  mov rcx (intconstant arity);
  call (subdef "m_allocate_closure");
  return ()
;;

let operator_name s =
  let rename_char = function
    | '+' -> "add"
    | '-' -> "sub"
    | '/' -> "div"
    | '*' -> "mul"
    | '>' -> "greater"
    | '<' -> "less"
    | '=' -> "equals"
    | _ -> "unknown"
  in
  explode s |> map rename_char |> String.concat "_" |> sprintf "m_op_%s"
;;

let load_name n =
  let* vars = variables in
  let* funs = fundefs in
  match SMap.find_opt n vars with
  | Some v -> mov rax v |> return
  | None when is_alphanumeric n |> not -> allocate_closure (operator_name n) 2
  | None when mem n funs ->
    let* arities = arities in
    let arity = SMap.find n arities in
    allocate_closure n arity
  | None -> return ()
;;

let load_int n = return @@ mov rax (intconstant n)

let compile_immexpr = function
  | ImmValue name -> load_name name
  | ImmNum n -> load_int n
  | ImmString _ -> load_int (-1)
  | ImmBool true -> load_int 1
  | ImmBool false -> load_int 0
  | ImmUnit -> load_int 0
;;

let append_immexpr e =
  let* res = compile_immexpr e in
  res;
  return ()
;;

let append_apply_argument target arg =
  comment "applying argument";
  let* () = append_immexpr target in
  push rax;
  sub rsp (intconstant 8);
  let* () = append_immexpr arg in
  mov rsi rax;
  add rsp (intconstant 8);
  pop rdi;
  call (subdef "m_apply");
  return ()
;;

let apply_store n r c =
  match SMap.find_opt n c.variable_bindings with
  | Some n -> mov n r
  | None -> ()
;;

let rec append_cexpr = function
  | CImm ie -> append_immexpr ie
  | CApp (target, argument) -> append_apply_argument target argument
  | CIfElse (condition, then_part, else_part) ->
    compile_if_then_else condition then_part else_part

and compile_if_then_else condition then_part else_part =
  let* branch_id = next_cnt in
  let* () = append_immexpr condition in
  let else_label = subdef (sprintf "if.else_%d" branch_id) in
  let end_label = subdef (sprintf "if.end_%d" branch_id) in
  cmp rax (intconstant 0);
  je else_label;
  let* () = append_block then_part in
  jmp end_label;
  label else_label;
  let* () = append_block else_part in
  label end_label;
  return ()

and evaluate_locals bindings c =
  let inner (name, expr) =
    let (_ : unit * context) = append_cexpr expr c in
    apply_store name rax c
  in
  List.iter inner bindings

and append_block (b : ablock) c =
  let expr, lets = b in
  evaluate_locals lets c;
  append_immexpr expr c
;;

let list_partititioni n lst =
  let fst = filteri (fun i _ -> i < n) lst in
  let snd = filteri (fun i _ -> i >= n) lst in
  fst, snd
;;

let truncate n lst = filteri (fun i _ -> i < n) lst
let zip l1 l2 = List.combine l1 l2

let map_stack bindings =
  let empty_map = SMap.empty in
  let stack_arguments_with_locations =
    mapi (fun i e -> e, StackLocation ((-i * 8) - 8)) bindings
  in
  let mapped_stack =
    fold_left
      (fun m a -> SMap.add (fst a) (snd a) m)
      empty_map
      stack_arguments_with_locations
  in
  mapped_stack
;;

let rec collect_local_ablock (_, lets) = concat_map collect_local_let lets
and collect_local_let (name, cexpr) = name :: collect_local_expr cexpr

and collect_local_expr = function
  | CIfElse (_, then_part, else_part) ->
    collect_local_ablock then_part @ collect_local_ablock else_part
  | _ -> []
;;

let store_arguments args c =
  let location_of n =
    match SMap.find_opt n c.variable_bindings with
    | Some v -> v
    | None -> Register "Not performed"
  in
  let register_arguments, stack_arguments = list_partititioni 6 args in
  let used_registers =
    [ rdi; rsi; rdx; rcx; r8; r9 ] |> truncate (length register_arguments)
  in
  iter2 (fun r s -> mov (location_of s) r) used_registers register_arguments;
  iter (fun arg -> pop (location_of arg)) stack_arguments
;;

let append_function_body (name, args, ablock) c =
  let all_local = collect_local_ablock ablock in
  let mapped_stack = map_stack (all_local @ args) in
  let c = { c with variable_bindings = mapped_stack } in
  append (sprintf "\n%s:" name);
  push rbp;
  mov rbp rsp;
  sub rsp (length (all_local @ args) * 16 |> intconstant);
  store_arguments args c;
  append_block ablock c |> fst;
  mov rsp rbp;
  pop rbp;
  ret ()
;;

let append_headers () =
  let externs =
    [ "m_allocate_closure"
    ; "m_apply"
    ; "m_malloc"
    ; "m_op_add"
    ; "m_op_sub"
    ; "m_op_mul"
    ; "m_op_div"
    ; "m_op_equals"
    ; "m_op_less"
    ; "m_op_less_equals"
    ; "m_op_greater"
    ; "m_op_greater_equals"
    ; "m_op_less_greater"
    ; "print_int"
    ; "print_bool"
    ]
  in
  iter (fun x -> append @@ "extern " ^ x) externs;
  append "global main"
;;

let triple_fst (a, _, _) = a

let asm fns ablock =
  let std_funs = [ "print_int", 1; "print_bool", 1 ] in
  let std_fun_names = map fst std_funs in
  let function_names = map triple_fst fns in
  let add_fun_arity m (name, args, _) = SMap.add name (length args) m in
  let function_arities = fold_left add_fun_arity SMap.empty fns in
  let function_arities =
    fold_left (fun m (n, a) -> SMap.add n a m) function_arities std_funs
  in
  let c =
    { variable_bindings = SMap.empty
    ; function_defenitions = std_fun_names @ function_names
    ; function_arities
    ; rsp_offset = 0
    ; counter = 0
    }
  in
  append_headers ();
  append_function_body ("main", [], ablock) c;
  iter (fun m -> append_function_body m c) fns;
  Buffer.contents generated
;;

let compile fns ablock =
  let create_newdir path perm = if not (Sys.file_exists path) then Sys.mkdir path perm in
  let build_dir = "compiled" in
  let make_exe program =
    create_newdir build_dir 0o777;
    let output = open_out (build_dir ^ "/program.asm") in
    Printf.fprintf output "%s" program;
    close_out output;
    let _ =
      Sys.chdir build_dir;
      Sys.command
        "gcc -c ../../stdlib/stdlib.c && nasm -f elf64 program.asm -o program.o && gcc \
         stdlib.o program.o -o program >/dev/null 2>&1"
    in
    ()
  in
  make_exe (asm fns ablock)
;;
