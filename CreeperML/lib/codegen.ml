(** Copyright 2023-2024, Arthur Alekseev and Starcev Matvey *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Llvm
open Anf.AnfTypeAst
open Type_ast.TypeAst
open Type_ast.TypeAstUtils
open Parser_ast.ParserAst
open Monad.Result

module Codegen = struct
  let contex = global_context ()
  let the_module = create_module contex "CreeperMLBestLenguage"
  let builder = builder contex

  module NamedValues = Map.Make (String)

  type return_type = { named_values : funvar NamedValues.t; value : llvalue }

  and funvar =
    | Var of anf_val_binding * (anf_val_binding -> return_type t)
    | Val of llvalue

  let value { named_values = _; value } = value
  let ret named_values value = { named_values; value } |> return

  module FunctionTypes = Map.Make (String)

  let float_type = float_type contex
  let bool_type = i32_type contex
  let integer_type = i32_type contex
  let string_type = array_type (i8_type contex) 1
  let unit_type = void_type contex
  let ptr = pointer_type contex
  let int_const = const_int integer_type
  let str_name n = typed_value n
  let load_int v = build_load integer_type v "loadint" builder
  let load_float v = build_load float_type v "loadfloat" builder

  let arity =
    let rec helper = function TyArrow (_, r) -> 1 + helper r | _ -> 0 in
    helper

  let alloc_closure f args arity =
    let argc = Array.length args in
    let argv = args in
    let argv_adr = build_malloc (array_type ptr argc) "arraymalloc" builder in
    let alloc i e =
      let addr =
        build_gep (array_type ptr argc) argv_adr
          [| int_const 0; int_const i |]
          "elem" builder
      in
      let (_ : Llvm.llvalue) = build_store e addr builder in
      ()
    in
    Array.iteri alloc argv;
    let arity = int_const arity in
    let alloc_t =
      function_type ptr [| ptr; integer_type; ptr; integer_type; integer_type |]
    in
    let alloc = declare_function "create_function" alloc_t the_module in
    build_call alloc_t alloc
      [| f; argc |> int_const; argv_adr; arity; arity |]
      "closurecreate" builder

  let put_to_ptr v =
    let addr = build_alloca (type_of v) "polytmp" builder in
    let (_ : Llvm.llvalue) = build_store v addr builder in
    addr

  let match_ptr f_matched f_else v =
    match type_of v |> classify_type with
    | TypeKind.Pointer -> f_matched v
    | _ -> f_else v

  let tmp_v v t =
    let new_addr = build_alloca t "tmpptr" builder in
    let data = build_load t v "tmp" builder in
    let (_ : Llvm.llvalue) = build_store data new_addr builder in
    new_addr

  let apply_closure cl argv argc =
    let apply_t = function_type ptr [| ptr; integer_type; ptr |] in
    let apply = declare_function "apply_args" apply_t the_module in
    build_call apply_t apply
      [| cl; int_const argc; argv |]
      "applyclosure" builder

  let try_find named_values name =
    match NamedValues.find_opt name named_values with
    | Some v -> (
        match v with
        | Var (bind, binder) -> binder bind
        | Val l -> ret named_values l)
    | None -> String.cat "Can't find value " name |> error

  let try_find_opt named_values name =
    match try_find named_values name with Ok r -> Some r | _ -> None

  let rec get_type = function
    | TyGround gr -> (
        match gr with
        | TInt -> integer_type
        | TString -> string_type
        | TFloat -> float_type
        | TBool -> bool_type
        | TUnit -> unit_type)
    | TyTuple ts -> List.map get_type ts |> Array.of_list |> struct_type contex
    | TyArrow _ as arr ->
        let args =
          let rec helper acc = function
            | TyArrow (arg, r) -> helper (get_type arg :: acc) r
            | t -> (List.rev acc, get_type t)
          in
          helper []
        in
        let args, r = args arr in
        function_type r (Array.of_list args)
    | TyVar _ -> ptr

  let rec rez_t = function TyArrow (_, rez) -> rez_t rez | t -> t

  let type_at n t =
    let rec helper = function
      | t, 0 -> t
      | TyArrow (_, r), n -> helper (r, n - 1)
      | t, _ -> t
    in
    helper (t, n)

  let codegen_imm named_values function_types = function
    | ImmLit t -> (
        match typed_value t with
        | LInt n -> const_int integer_type n |> return
        | LFloat n -> const_float float_type n |> return
        | LBool b -> const_int bool_type (if b then 1 else 0) |> return
        | LUnit -> const_pointer_null unit_type |> return
        | LString str ->
            let len = String.length str in
            let* str =
              if len < 2 then error "Lenght of string withs quotes less that 2"
              else len - 2 |> String.sub str 1 |> return
            in
            build_global_stringptr str "" builder |> return)
    | ImmVal t -> (
        let name = str_name t in
        match
          (lookup_function name the_module, try_find_opt named_values name)
        with
        | _, Some f -> return f.value
        | Some f, _ ->
            FunctionTypes.find name function_types
            |> arity |> alloc_closure f [||] |> return
        | _ ->
            Printf.sprintf "Can't find function/value at number %s" name
            |> error)

  let codegen_sig named_values function_types { value = name; typ = t } args =
    let args_ts = List.map typ args in
    let args_names = List.map str_name args in
    let ft = Array.make (List.length args) ptr |> function_type ptr in
    let* f =
      match lookup_function name the_module with
      | None -> declare_function name ft the_module |> return
      | Some f -> (
          match block_begin f with
          | At_end f when type_of f |> element_type = ft -> return f
          | _ -> error "redefinition of function")
    in
    let orig_ft = rez_t t |> List.fold_right ty_arrow args_ts in
    let function_types =
      List.fold_left2
        (fun function_types name -> function
          | TyArrow _ as t -> FunctionTypes.add name t function_types
          | _ -> function_types)
        (FunctionTypes.add name orig_ft function_types)
        args_names args_ts
    in
    let named_values =
      List.fold_left2
        (fun named_values e name ->
          set_value_name name e;
          NamedValues.add name (Val e) named_values)
        named_values
        (params f |> Array.to_list)
        args_names
    in
    return ({ named_values; value = f }, function_types)

  let codegen_predef name argv =
    let op f argv load_f =
      let lhs = load_f argv.(0) in
      let rhs = load_f argv.(1) in
      f lhs rhs "op" builder |> return
    in
    let bin_op f argv load_f =
      let lhs = load_f argv.(0) in
      let rhs = load_f argv.(1) in
      let i = f lhs rhs "cmptmp" builder in
      build_zext i integer_type "booltmp" builder |> return
    in
    match name with
    | "-" -> op build_sub argv load_int
    | "+" -> op build_add argv load_int
    | "*" -> op build_mul argv load_int
    | "/" -> op build_sdiv argv load_int
    | "<=" -> bin_op (build_icmp Icmp.Sle) argv load_int
    | "<" -> bin_op (build_icmp Icmp.Slt) argv load_int
    | "==" -> bin_op (build_icmp Icmp.Eq) argv load_int
    | ">" -> bin_op (build_icmp Icmp.Sgt) argv load_int
    | ">=" -> bin_op (build_icmp Icmp.Sge) argv load_int
    | "-." -> op build_fsub argv load_float
    | "+." -> op build_fadd argv load_float
    | "*." -> op build_fmul argv load_float
    | "/." -> op build_fdiv argv load_float
    | "<=." -> bin_op (build_fcmp Fcmp.Ole) argv load_float
    | "<." -> bin_op (build_fcmp Fcmp.Olt) argv load_float
    | "==." -> bin_op (build_fcmp Fcmp.Oeq) argv load_float
    | ">." -> bin_op (build_fcmp Fcmp.Olt) argv load_float
    | ">=." -> bin_op (build_fcmp Fcmp.Olt) argv load_float
    | "print_int" ->
        let ft = function_type unit_type [| integer_type |] in
        let f = declare_function "bin_print_int" ft the_module in
        let arg = load_int argv.(0) in
        build_call ft f [| arg |] "" builder |> return
    | "print_string" ->
        let ft = function_type unit_type [| ptr |] in
        let f = declare_function "print_string_raw" ft the_module in
        build_call ft f [| argv.(0) |] "" builder |> return
    | name -> Printf.sprintf "fail predef ar %s" name |> error

  let codegen_predef_function named_values function_types op =
    let name = str_name op in
    let args =
      let rec helper acc n = function
        | TyArrow (l, r) ->
            let name = Printf.sprintf "%s_%d" name n in
            helper (with_typ r name :: acc) (n + 1) l
        | _ -> acc
      in
      typ op |> helper [] 1 |> List.rev
    in
    let* { named_values; value = f }, function_types =
      codegen_sig named_values function_types op args
    in
    let bb = append_block contex "entry" f in
    position_at_end bb builder;
    let* argv =
      monadic_map args (fun a -> try_find named_values (str_name a) >>| value)
      >>| Array.of_list
    in
    let* ret_val = codegen_predef name argv in
    let ret_val =
      match (typ op |> rez_t, type_of ret_val |> classify_type) with
      | TyGround TUnit, _ -> const_pointer_null ptr
      | _, TypeKind.Pointer -> ret_val
      | _ -> put_to_ptr ret_val
    in
    let (_ : Llvm.llvalue) = build_ret ret_val builder in
    return (named_values, function_types)

  type tmp = TMP | NOTMP

  let rec codegen_expr named_values function_types =
    let apply_to_closure f args tmp =
      let* cl = codegen_imm named_values function_types (ImmVal f) in
      let argc = List.length args in
      let* argv =
        monadic_map args (fun a ->
            codegen_imm named_values function_types a
            >>| match_ptr (fun a -> a) put_to_ptr)
        >>| Array.of_list
      in
      let argv_adr = build_malloc (array_type ptr argc) "arraymalloc" builder in
      let alloc i e =
        let addr =
          build_gep (array_type ptr argc) argv_adr
            [| int_const 0; int_const i |]
            "elem" builder
        in
        let (_ : Llvm.llvalue) = build_store e addr builder in
        ()
      in
      Array.iteri alloc argv;
      let rz = apply_closure cl argv_adr argc in
      (match (typ f |> type_at argc, tmp) with
      | TyGround TUnit, _ | TyArrow _, _ -> rz
      | t, TMP -> get_type t |> tmp_v rz
      | _ -> rz)
      |> ret named_values
    in
    function
    | AImm imm ->
        codegen_imm named_values function_types imm >>= ret named_values
    | ATuple ims ->
        let* es =
          monadic_map ims (codegen_imm named_values function_types)
          >>| Array.of_list
        in
        let t = Array.map type_of es |> struct_type contex in
        let addr = build_malloc t "tuplemalloc" builder in
        let alloc i e =
          let addr =
            build_gep t addr [| int_const 0; int_const i |] "elem" builder
          in
          let (_ : Llvm.llvalue) = build_store e addr builder in
          ()
        in
        Array.iteri alloc es;
        ret named_values addr
    | AApply (ImmVal f, args) -> apply_to_closure f args TMP
    | AClosure (f, args) -> apply_to_closure f args NOTMP
    | Aite (cond, { lets = then_lets; res = tr }, { lets = else_lets; res = fl })
      ->
        let named_vs lets =
          monadic_fold
            (fun acc l ->
              let* { value = _; named_values } =
                codegen_local_var acc function_types l
              in
              return named_values)
            named_values lets
        in

        let curr_block = insertion_block builder in
        let fun_block = block_parent curr_block in
        let test_block = append_block contex "test" fun_block in
        let then_block = append_block contex "then" fun_block in
        let else_block = append_block contex "else" fun_block in
        let merge_block = append_block contex "merge" fun_block in

        let (_ : Llvm.llvalue) = build_br test_block builder in
        position_at_end test_block builder;
        let* cond =
          codegen_imm named_values function_types cond
          >>| match_ptr load_int (fun c -> c)
        in
        let cond_val = build_icmp Icmp.Eq cond (int_const 0) "cond" builder in
        let (_ : Llvm.llvalue) =
          build_cond_br cond_val else_block then_block builder
        in

        position_at_end then_block builder;
        let* named_values = named_vs then_lets in
        let* then_val = codegen_imm named_values function_types tr in
        let new_then_block = insertion_block builder in

        position_at_end else_block builder;
        let* named_values = named_vs else_lets in
        let* else_val = codegen_imm named_values function_types fl in
        let new_else_block = insertion_block builder in

        let addr =
          insertion_block builder |> block_parent |> entry_block |> instr_begin
          |> builder_at contex
          |> build_alloca (type_of else_val) "ifrezptr"
        in
        position_at_end new_then_block builder;
        let (_ : Llvm.llvalue) = build_store then_val addr builder in
        let (_ : Llvm.llvalue) = build_br merge_block builder in
        position_at_end new_else_block builder;
        let (_ : Llvm.llvalue) = build_store else_val addr builder in
        let (_ : Llvm.llvalue) = build_br merge_block builder in

        position_at_end merge_block builder;
        let rz = build_load (type_of else_val) addr "ifrez" builder in
        ret named_values rz
    | ATupleAccess (ImmVal name, ix) ->
        let n = str_name name in
        let* str = try_find named_values n in
        let t = typ name |> get_type in
        let addr =
          build_gep t str.value [| int_const 0; int_const ix |] "access" builder
        in

        build_load (Array.get (subtypes t) ix) addr "loadaccesss" builder
        |> ret named_values
    | AApply (ImmLit _, _) | ATupleAccess (ImmLit _, _) ->
        error
          {|Case of over-representation of anf-ast. It never occurs because of
            typecheck|}

  and codegen_local_var named_values function_types { name; e } =
    let name = str_name name in
    let* { value = body; named_values } =
      codegen_expr named_values function_types e
    in
    let r =
      match_ptr
        (fun body -> body)
        (fun body ->
          let addr = put_to_ptr body in
          build_load (type_of body) addr "polyload" builder)
        body
    in
    let named_values = NamedValues.add name (Val r) named_values in
    ret named_values r

  let codegen_anf_binding named_values function_types main = function
    | AnfVal ({ name; e } as bind) ->
        let* named_values =
          match (typ name, e) with
          | TyGround TUnit, AImm (ImmVal { value = _; typ = TyGround TUnit }) ->
              return named_values
          | TyGround TUnit, _ ->
              position_at_end main builder;
              let* { named_values; value = _ } =
                codegen_expr named_values function_types e
              in
              return named_values
          | _ ->
              NamedValues.add (str_name name)
                (Var (bind, codegen_local_var named_values function_types))
                named_values
              |> return
        in
        return (named_values, function_types)
    | AnfFun { name; args; env; body = { lets; res = body } } ->
        let* { named_values; value = f }, function_types =
          env @ args |> codegen_sig named_values function_types name
        in
        let bb = append_block contex "entry" f in
        position_at_end bb builder;
        let* named_values =
          monadic_fold
            (fun acc l ->
              let* { named_values; value = _ } =
                codegen_local_var acc function_types l
              in
              return named_values)
            named_values lets
        in
        let* ret_val = codegen_imm named_values function_types body in
        let ret_val =
          match (typ name |> rez_t, type_of ret_val |> classify_type) with
          | TyGround TUnit, _ -> const_pointer_null ptr
          | _, TypeKind.Pointer -> ret_val
          | _ -> put_to_ptr ret_val
        in
        let (_ : Llvm.llvalue) = build_ret ret_val builder in
        return (named_values, function_types)

  let codegen_ret_main b =
    position_at_end b builder;
    build_ret (int_const 0) builder

  let codegen_main =
    let dec = function_type integer_type [||] in
    declare_function "main" dec the_module |> append_block contex "entry"

  let top_lvl code =
    let b = codegen_main in
    let* env =
      monadic_fold
        (fun (named_values, function_types) op ->
          codegen_predef_function named_values function_types op)
        (NamedValues.empty, FunctionTypes.empty)
        Std.Std.operators
    in
    let* _ =
      monadic_fold
        (fun (named_values, function_types) cmd ->
          codegen_anf_binding named_values function_types b cmd)
        env code
    in
    let _ = codegen_ret_main b in
    return ()

  let dmp_code file = print_module file the_module

  let compile code name =
    let output_ll = Printf.sprintf "%s-opt.ll" name in
    let output_opt_ll = Printf.sprintf "%s-opt.ll" name in
    let output_opt_s = Printf.sprintf "%s-opt.s" name in
    let (_ : unit) =
      match top_lvl code with
      | Error err -> print_endline err
      | _ -> dmp_code output_ll
    in
    let _ =
      Printf.sprintf "llc --relocation-model=pic %s" output_opt_ll
      |> Sys.command
    in
    let _ =
      Printf.sprintf "gcc %s lib/bindings.c -o %s" output_opt_s name
      |> Sys.command
    in
    let _ = Printf.sprintf "./%s" name |> Sys.command in
    let _ =
      Printf.sprintf "rm %s %s %s %s" name output_ll output_opt_ll output_opt_s
      |> Sys.command
    in
    ()
end
