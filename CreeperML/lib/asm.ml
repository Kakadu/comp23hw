module Asm = struct
  open Anf.AnfTypeAst
  open Type_ast.TypeAst
  open Parser_ast.ParserAst
  open Std
  open Monad.Result

  let reg_size = 16
  let ptr_size = 8

  type reg = string

  type storage =
    | Stack of int
    | Reg of reg
    | IntConst of int
    | Displacement of string * int
    | Fndef of string

  type instruction =
    | Mov of storage * storage
    | Movzx of storage * storage
    | Add of storage * storage
    | Sub of storage * storage
    | Imul of storage * storage
    | Idiv of storage
    | Call of string
    | Push of storage
    | Pop of storage
    | Int of int
    | Ret
    | Jmp of string
    | Je of string
    | Jne of string
    | Cmp of storage * storage
    | Setge of storage
    | Setg of storage
    | Setle of storage
    | Setl of storage
    | Sete of storage
    | Label of string (* bad *)
    | Cqo

  type fn = { name : string; body : instruction list }

  let rax = Reg "rax"
  let rdi = Reg "rdi"
  let rsi = Reg "rsi"
  let al = Reg "al"
  let eax = Reg "eax"
  let rsp = Reg "rsp"
  let rbp = Reg "rbp"
  let rdx = Reg "rdx"
  let rcx = Reg "rcx"
  let r8 = Reg "r8"
  let r9 = Reg "r9"
  let r12 = Reg "r12"
  let pop l = Pop l
  let add r1 r2 = Add (r1, r2)
  let imul r1 r2 = Imul (r1, r2)
  let sub r1 r2 = Sub (r1, r2)
  let idiv r = Idiv r
  let push l = Push l
  let ic i = IntConst i
  let mov r1 r2 = Mov (r1, r2)
  let setle r = Setle r
  let setl r = Setl r
  let setge r = Setge r
  let setg r = Setg r
  let sete r = Sete r
  let movzx r1 r2 = Movzx (r1, r2)
  let cmp r1 r2 = Cmp (r1, r2)
  let call r = Call r
  let je x = Je x
  let cqo = Cqo
  let dp reg d = Displacement (reg, d)

  module SMap = Map.Make (String)

  type compiler_info = {
    memory : storage SMap.t;
    functions : string list;
    arities : int SMap.t;
  }

  let latest_if_id = ref 0

  let gen_next_if_id () =
    latest_if_id := !latest_if_id + 1;
    !latest_if_id

  let ( >> ) x y = x @ [ y ]
  let ( >>> ) = ( @ )
  let ( =>> ) x y = x >>| fun x -> x @ [ y ]
  let ( =>>> ) x y = x >>| fun x -> x @ y
  let ( ->>> ) x y = y >>| fun y -> x @ y

  let ( |>>> ) x y =
    x >>= fun x ->
    y >>| fun y -> x @ y

  let lst x = [ x ]
  let concat = List.concat

  let rec arity_of t =
    match t with TyArrow (_, t2) -> 1 + arity_of t2 | _ -> 0

  let std_functions = Std.operators |> List.map (fun x -> x.value)
  let sizeof _ = 8
  let align16 x = if Int.rem x 16 <> 0 then x - Int.rem x 16 + 16 else x
  let sum_by f = List.fold_left (fun xs x -> xs + f x) 0

  (*  %rdi, %rsi, %rdx, %rcx, %r8, and %r9 then stack *)
  let get_arg_loc = function
    | 0 -> Some rdi
    | 1 -> Some rsi
    | 2 -> Some rdx
    | 3 -> Some rcx
    | 4 -> Some r8
    | 5 -> Some r9
    | _ -> None

  let split_args lst =
    let rec helper lst i (acc1, acc2) =
      match (lst, get_arg_loc i) with
      | [], _ -> (acc1, acc2)
      | hd :: tl, Some _ -> helper tl (i + 1) (hd :: acc1, acc2)
      | hd :: tl, None -> helper tl (i + 1) (acc1, hd :: acc2)
    in
    helper lst 0 ([], [])

  let rec collect_locals l =
    match l.e with
    | Aite (_, t, e) ->
        List.concat_map collect_locals t.lets
        @ List.concat_map collect_locals e.lets
        @ [ l.name ]
    | _ -> [ l.name ]

  let map_fn_stack (fn : anf_fun_binding) =
    let empty = SMap.empty in
    let inner sign (mm, li) x =
      (SMap.add x.value (Stack li) mm, sign li (sizeof x.typ))
    in
    let reg_args, stack_args = split_args (fn.env @ fn.args) in
    let all_local_vars = List.concat_map collect_locals fn.body.lets in
    let args, last = List.fold_left (inner ( - )) (empty, -reg_size) reg_args in
    let args, _ = List.fold_left (inner ( + )) (args, reg_size) stack_args in
    List.fold_left (inner ( - )) (args, last) all_local_vars |> fst

  let compile_fn_call fn args =
    let stack_len =
      List.filteri (fun i _ -> Option.is_none (get_arg_loc i)) args
      |> List.fold_left (fun xs x -> xs + sizeof x) 0
    in
    let stack_fix = [ add rsp (stack_len |> align16 |> ic) ] in
    match fn with
    (* Inline function call here *)
    | "-" -> return [ sub rdi rsi; mov rax rdi ] (* - *)
    | "+" -> return [ add rdi rsi; mov rax rdi ] (* + *)
    | "*" -> return [ imul rdi rsi; mov rax rdi ] (* * *)
    | "/" -> return [ mov rax rdi; cqo; idiv rsi ] (* / *)
    | "<=" -> return [ cmp rdi rsi; setle al; movzx eax al ] (* <= *)
    | "<" -> return [ cmp rdi rsi; setl al; movzx eax al ] (* < *)
    | "==" -> return [ cmp rdi rsi; sete al; movzx eax al ] (* == *)
    | ">" -> return [ cmp rdi rsi; setg al; movzx eax al ] (* > *)
    | ">=" -> return [ cmp rdi rsi; setge al; movzx eax al ] (* >= *)
    | "print_int" -> [ call "bin_print_int" ] >>> stack_fix |> return
    | x when String.ends_with ~suffix:"." x ->
        error "Compiler error: Float point functions not done"
    | other -> [ call other ] >>> stack_fix |> return

  let int_of_literal x =
    match x.value with
    | LInt x -> return x
    | LBool true -> return 1
    | LBool false -> return 0
    | LUnit -> return 0
    | LString _ -> error "Compiler error: Strings are not supported yet"
    | LFloat _ -> error "Compiler error: Floats are not supported yet"

  let preserve_reg reg prog =
    [ push reg; sub rsp (ic 8) ] >>> prog >>> [ add rsp (ic 8); pop reg ]

  let mem_imm cinfo imm =
    match imm with
    | ImmVal v when List.mem v.value cinfo.functions -> Fndef v.value |> return
    | ImmVal x -> (
        match SMap.find_opt x.value cinfo.memory with
        | Some x -> return x
        | None -> error "Compiler error: Could not find variable in memory")
    | ImmLit l -> int_of_literal l >>| ic

  let rename_fn name = match name with "print_int" -> "bin_print_int" | e -> e

  let alloc_closure cinfo fn args =
    let arity = SMap.find fn.value cinfo.arities in
    let fn_name = rename_fn fn.value in
    let load_size = mov rdi (arity * ptr_size |> ic) in
    let create_env_ptr = [ call "cm_malloc"; mov r12 rax ] in
    let inner i arg =
      mem_imm cinfo arg >>| mov r9 >>| lst =>> mov (dp "r12" (i * ptr_size)) r9
    in
    let* store_args =
      preserve_reg r9 |<< (monadic_mapi args inner >>| List.concat)
    in
    let load_argv = mov rdx r12 in
    let load_argc = mov rsi (List.length args |> ic) in
    let load_fn = mov rdi (Fndef fn_name) in
    let load_arity = mov rcx (arity |> ic) in
    let create_closure = call "create_function" in
    preserve_reg r12
      ([ load_size ] >>> create_env_ptr >>> store_args >> load_argv >> load_argc
     >> load_fn >> load_arity >> create_closure)
    |> return

  let ld_imm cinfo imm =
    match imm with
    | ImmVal v when List.mem v.value cinfo.functions -> alloc_closure cinfo v []
    | ImmVal v when List.mem v.value std_functions -> alloc_closure cinfo v []
    | _ -> mem_imm cinfo imm >>| fun x -> [ mov rax x ]

  let fix_align_16 instructions size =
    instructions @ [ sub rsp (Int.rem size 16 |> ic) ]

  let compile_push_args cinfo (args : imm list) =
    let inner (xs, arg_i, s) arg =
      let* loc = mem_imm cinfo arg in
      match get_arg_loc arg_i with
      | Some reg -> (xs @ [ Mov (reg, loc) ], arg_i + 1, s) |> return
      | None ->
          let* arg_load = ld_imm cinfo arg in
          (xs @ arg_load, arg_i + 1, s + sizeof arg) |> return
    in
    let* instr, _, size = monadic_fold inner ([], 0, 0) args in
    fix_align_16 instr size |> return

  let rec compile_expr cinfo (e : anf_expr) =
    match e with
    | AApply (ImmVal fn, args)
      when List.mem fn.value std_functions
           && List.length args == arity_of fn.typ ->
        compile_push_args cinfo args |>>> compile_fn_call fn.value args
    | AApply (ImmVal fn, args) ->
        let argc = List.length args in
        let alloc_arr =
          [ mov rdi (argc * ptr_size |> ic); call "cm_malloc"; mov r12 rax ]
        in
        let store_arg i arg =
          ld_imm cinfo arg =>> mov (dp "r12" (i * ptr_size)) rax
        in
        let* store_args = monadic_mapi args store_arg >>| concat in
        let push_array = [ push r12; sub rsp (ic 8) ] in
        let* load_self =
          if List.mem fn.value cinfo.functions then
            alloc_closure cinfo fn [] =>> mov rdi rax
          else mem_imm cinfo (ImmVal fn) >>| mov rax >>| lst =>> mov rdi rax
        in
        let load_argc = mov rsi (argc |> ic) in
        let pop_argv = [ add rsp (ic 8); pop rdx ] in
        let call_apply = call "apply_args" in
        alloc_arr >>> store_args >>> push_array >>> load_self >> load_argc
        >>> pop_argv >> call_apply |> return
    | AImm i -> ld_imm cinfo i
    | Aite (i, t, e) ->
        let compile_block x = monadic_map x (compile_vb cinfo) >>| concat in
        let if_id = gen_next_if_id () in
        let mklabel name = Label (Format.sprintf "%s_%d" name if_id) in
        let cont = mklabel "cont" in
        let jmp_to_cont = Jmp (Format.sprintf "cont_%d" if_id) in
        let then_branch =
          compile_block t.lets |>>> ld_imm cinfo t.res =>> jmp_to_cont
        in
        let else_branch =
          [ mklabel "else" ] ->>> compile_block e.lets
          |>>> ld_imm cinfo e.res =>> cont
        in
        let* if_part =
          ld_imm cinfo i
          =>>> [ cmp rax (ic 0); je (Format.sprintf "else_%d" if_id) ]
        in
        if_part ->>> then_branch |>>> else_branch
    | AClosure (fn, args) -> alloc_closure cinfo fn args
    | ATuple elements ->
        let argc = List.length elements in
        let alloc_tuple =
          [ mov rdi (argc * ptr_size |> ic); call "cm_malloc"; mov r12 rax ]
        in
        let inner i arg =
          ld_imm cinfo arg =>> mov (dp "r12" (i * ptr_size)) rax
        in
        let store_elements = monadic_mapi elements inner >>| concat in
        preserve_reg r12 |<< (alloc_tuple ->>> store_elements =>> mov rax r12)
    | ATupleAccess (tuple, index) ->
        preserve_reg r9
        |<< (mem_imm cinfo tuple >>| mov r9 >>| lst
            =>> push (dp "r9" (index * ptr_size))
            =>> pop rax)
    | AApply (ImmLit _, _) -> error "Compiler error: Cannot apply literal"

  and compile_vb cinfo (vb : anf_val_binding) =
    let store =
      match vb.name.typ with
      | TyGround TUnit -> []
      | _ -> SMap.find vb.name.value cinfo.memory |> fun x -> [ mov x rax ]
    in
    let* expr = compile_expr cinfo vb.e in
    expr >>> store |> return

  let rec collect_expr_size (e : anf_val_binding) =
    match e.e with
    | Aite (_, t, e) ->
        (sum_by collect_expr_size) t.lets + (sum_by collect_expr_size) e.lets
    | _ -> sizeof e.name

  let add_fn_stuff x =
    let save_base = [ push rbp; mov rbp rsp ] in
    let ret = [ mov rsp rbp; pop rbp; Ret ] in
    save_base >>> x >>> ret

  let compile_fn cinfo (fn : anf_fun_binding) =
    let name = fn.name.value in
    let cinfo = { cinfo with memory = map_fn_stack fn } in
    let stack_len =
      (sum_by sizeof) fn.args
      + (sum_by sizeof) fn.env
      + (sum_by collect_expr_size) fn.body.lets
      + sizeof fn.body.res
    in
    let sub_esp = sub rsp (stack_len + reg_size |> align16 |> ic) in
    let* body = monadic_map fn.body.lets (compile_vb cinfo) >>| concat in
    let* res = mem_imm cinfo fn.body.res >>| mov rax in
    let save_args =
      let rec helper lst i instrs =
        match (lst, get_arg_loc i) with
        | [], _ -> instrs
        | hd :: tl, Some reg ->
            Mov (SMap.find hd.value cinfo.memory, reg) :: instrs
            |> helper tl (i + 1)
        | _, None -> instrs
      in
      helper (fn.env >>> fn.args) 0 []
    in
    return
      { name; body = add_fn_stuff ([ sub_esp ] >>> save_args >>> body >> res) }

  let main_fn body =
    {
      args = [];
      env = [];
      name = { value = "main"; typ = TyGround TInt };
      body =
        { lets = body; res = ImmLit { value = LInt 0; typ = TyGround TInt } };
    }

  let compile ast : (fn list, string) Result.t =
    let fn_defs, main_fn_body =
      List.partition_map
        (function AnfFun fn -> Left fn | AnfVal v -> Right v)
        ast
    in
    let l = List.length in
    let arities =
      List.map
        (fun (fn : anf_fun_binding) -> (fn.name.value, l fn.args + l fn.env))
        fn_defs
      |> List.to_seq
      |> Seq.append
           (Std.operators
           |> List.map (fun x -> (x.value, arity_of x.typ))
           |> List.to_seq)
      |> SMap.of_seq
    in
    let cinfo =
      {
        functions = List.map (fun (x : anf_fun_binding) -> x.name.value) fn_defs;
        memory = SMap.empty;
        arities;
      }
    in
    let* main_fn =
      main_fn main_fn_body |> compile_fn cinfo >>| fun x ->
      { x with name = "main" }
    in
    let* all_fns = monadic_map fn_defs (compile_fn cinfo) in
    main_fn :: all_fns |> return
end

module AsmRenderer = struct
  open Asm

  let header =
    {|
extern bin_print_int
extern print_string
extern create_function
extern apply_args
extern cm_malloc

global main
|}

  let rm = function
    | Stack d when d > 0 -> Format.sprintf "qword [rbp+%d]" d
    | Stack d when d < 0 -> Format.sprintf "qword [rbp%d]" d
    | Stack _ -> Format.sprintf "qword [rbp]"
    | Reg r -> r
    | IntConst c -> Format.sprintf "%d" c
    | Displacement (r, d) when d > 0 -> Format.sprintf "qword [%s+%d]" r d
    | Displacement (r, d) when d < 0 -> Format.sprintf "qword [%s%d]" r d
    | Displacement (r, _) -> Format.sprintf "qword [%s]" r
    | Fndef fname -> fname

  let not_reg = function Reg _ -> false | _ -> true

  let render_instr = function
    | Mov (src, dst) when not_reg src && not_reg dst ->
        Format.sprintf "mov r9, %s\n  mov %s, r9" (rm dst) (rm src)
    | Mov (src, dst) -> Format.sprintf "mov %s, %s" (rm src) (rm dst)
    | Movzx (src, dst) -> Format.sprintf "movzx %s, %s" (rm src) (rm dst)
    | Add (src, dst) -> Format.sprintf "add %s, %s" (rm src) (rm dst)
    | Sub (src, dst) -> Format.sprintf "sub %s, %s" (rm src) (rm dst)
    | Cmp (src, dst) -> Format.sprintf "cmp %s, %s" (rm src) (rm dst)
    | Imul (src, dst) -> Format.sprintf "imul %s, %s" (rm src) (rm dst)
    | Idiv r -> Format.sprintf "idiv %s" (rm r)
    | Call fn -> Format.sprintf "call %s" fn
    | Push src -> rm src |> Format.sprintf "push %s"
    | Pop dst -> rm dst |> Format.sprintf "pop %s"
    | Int _ -> "int 0x80"
    | Ret -> "ret"
    | Label m -> Format.sprintf "%s:" m
    | Jmp d -> Format.sprintf "jmp %s" d
    | Je d -> Format.sprintf "je %s" d
    | Jne d -> Format.sprintf "jne %s" d
    | Setge d -> Format.sprintf "setge %s" (rm d)
    | Setle d -> Format.sprintf "setle %s" (rm d)
    | Setl d -> Format.sprintf "setl %s" (rm d)
    | Setg d -> Format.sprintf "setg %s" (rm d)
    | Sete d -> Format.sprintf "sete %s" (rm d)
    | Cqo -> "cqo"

  let render_instrs il =
    let add_offset s = Format.sprintf "  %s" s in
    List.map render_instr il |> List.map add_offset |> String.concat "\n"

  let render_fn p =
    let nm_dec = Format.sprintf "%s:\n" p.name in
    Format.sprintf "%s%s" nm_dec (render_instrs p.body)

  let render (p : fn list) =
    List.map render_fn p |> String.concat "\n\n" |> Format.sprintf "%s%s" header
end

module AsmOptimizer = struct
  open Asm

  let meaningful_instr = function
    | Mov (x, y) when x = y -> false
    | Add (_, IntConst 0) -> false
    | Sub (_, IntConst 0) -> false
    | _ -> true

  let optimize_fn fn = { fn with body = List.filter meaningful_instr fn.body }
  let optimize = List.map optimize_fn
end

module Build = struct
  let create_newdir path perm =
    if not (Sys.file_exists path) then Sys.mkdir path perm

  let build_dir = "cm_build"

  let make_exe build_loc args program =
    let () = create_newdir build_dir 0o727 in
    let asm_out = open_out (build_dir ^ "/program.asm") in
    let () = Printf.fprintf asm_out "%s" (AsmRenderer.render program) in
    let () = close_out asm_out in
    let _ = Sys.command (build_loc ^ args) in
    ()
end
