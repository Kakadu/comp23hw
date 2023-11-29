(** Copyright 2023-2024, Arthur Alekseev and Starcev Matvey *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Llvm
open Anf.AnfTypeAst
open Type_ast.TypeAst
open Type_ast.TypeAstUtils
open Parser_ast.ParserAst
open Monad.Result

module Codegen = struct
  type funvar =
    | Func of anf_val_binding * (anf_val_binding -> llvalue t)
    | Val of llvalue

  let contex = global_context ()
  let the_module = create_module contex "CreeperMLBestLenguage"
  let builder = builder contex
  let named_values : (string, funvar) Hashtbl.t = Hashtbl.create 42
  let function_types : (string, lltype) Hashtbl.t = Hashtbl.create 42
  let float_type = float_type contex
  let bool_type = i32_type contex
  let integer_type = i32_type contex
  let string_type = array_type (i8_type contex) 1
  let unit_type = void_type contex
  let int_const = const_int integer_type
  let str_name n = typed_value n |> string_of_int

  let try_find name msg =
    try
      Hashtbl.find named_values name |> function
      | Func (bind, binder) ->
          binder bind |> fun r ->
          Hashtbl.remove named_values name;
          r
      | Val l -> return l
    with Not_found -> error msg

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
        let rec args = function
          | TyArrow (arg, r) -> args r |> fun (ars, r) -> (arg :: ars, r)
          | t -> ([], t)
        in
        let args, r = args arr in
        function_type (get_type r) (List.map get_type args |> Array.of_list)
    | _ -> pointer_type contex

  let codegen_imm = function
    | ImmLit t ->
        (match typed_value t with
        | LInt n -> const_int integer_type n
        | LFloat n -> const_float float_type n
        | LBool b -> const_int bool_type (if b then 1 else 0)
        | LString str -> const_string contex str
        | LUnit -> const_pointer_null unit_type)
        |> return
    | ImmVal t ->
        let name = str_name t in
        try_find name
          (Printf.sprintf "Can't find function/value at number %s" name)

  let codegen_sig { value = name; typ } args =
    let name = Printf.sprintf "f%d" name in
    let ft = get_type typ in
    let* f =
      match lookup_function name the_module with
      | None -> declare_function name ft the_module |> return
      | Some f ->
          if block_begin f <> At_end f then error "redefinition of function"
          else if element_type (type_of f) <> ft then
            error "redefinition of function"
          else return f
    in
    Hashtbl.add function_types name ft;
    if List.length args > 0 then
      Array.iteri
        (fun i e ->
          let n = List.nth args i |> str_name in
          set_value_name n e;
          Hashtbl.add named_values n (Val e))
        (params f)
    else ();
    return f

  let codegen_predef name args =
    match name with
    | "f1" (* <= *) ->
        let* lhs = List.hd args |> codegen_imm in
        let* rhs = List.nth args 1 |> codegen_imm in
        let i = build_icmp Icmp.Sle lhs rhs "cmptmp" builder in
        build_zext i integer_type "booltmp" builder |> return
    | "f2" (* - *) ->
        let* lhs = List.hd args |> codegen_imm in
        let* rhs = List.nth args 1 |> codegen_imm in
        build_sub lhs rhs "subtmp" builder |> return
    | "f3" (* * *) ->
        let* lhs = List.hd args |> codegen_imm in
        let* rhs = List.nth args 1 |> codegen_imm in
        build_mul lhs rhs "multmp" builder |> return
    | "f4" (* + *) ->
        let* lhs = List.hd args |> codegen_imm in
        let* rhs = List.nth args 1 |> codegen_imm in
        build_add lhs rhs "addtmp" builder |> return
    | "f5" ->
        let f =
          declare_function "print_int"
            (function_type (void_type contex) [| integer_type |])
            the_module
        in
        let* arg = List.hd args |> codegen_imm in
        build_call
          (function_type (void_type contex) [| integer_type |])
          f [| arg |] "" builder
        |> return (* TODO perenesti iz predefa *)
    | _ -> error "fail predef"

  let rec rez_t = function TyArrow (_, rez) -> rez_t rez | t -> t

  let rec codegen_expr = function
    | AImm imm -> codegen_imm imm
    | ATuple ims ->
        let* es = monadic_map ims codegen_imm >>| Array.of_list in
        let t = Array.map type_of es |> struct_type contex in
        let addr = build_malloc t "tuplemalloc" builder in
        let alloc i e =
          let addr =
            build_gep t addr [| int_const 0; int_const i |] "elem" builder
          in
          ignore (build_store e addr builder)
        in
        Array.iteri alloc es;
        return addr
    | AApply (ImmVal f, args) -> (
        let rez_t, callname =
          typ f |> rez_t |> fun t ->
          (match t with TyGround TUnit -> "" | _ -> "calltmp") |> fun name ->
          (get_type t, name)
        in
        let name = typed_value f |> Printf.sprintf "f%d" in
        match lookup_function name the_module with
        | None -> codegen_predef name args
        | Some f ->
            let params = params f in
            if List.length args != Array.length params then
              error "Count of args and arrnost' of function are not same"
            else
              let args =
                List.map2
                  (fun a p ->
                    match type_of p |> classify_type with
                    | TypeKind.Pointer ->
                        codegen_imm a >>| fun a ->
                        let addr = build_alloca (type_of a) "polytmp" builder in
                        ignore (build_store a addr builder);
                        addr
                    | _ -> codegen_imm a)
                  args (Array.to_list params)
              in
              let* args =
                List.fold_right
                  (fun a acc ->
                    let* a = a in
                    let* acc = acc in
                    a :: acc |> return)
                  args (return [])
                >>| Array.of_list
              in
              let* fun_t =
                try Hashtbl.find function_types name |> return
                with _ -> Printf.sprintf "can't find type of %s" name |> error
              in
              let rz = build_call fun_t f args callname builder in
              (match return_type fun_t |> classify_type with
              | TypeKind.Pointer -> build_load rez_t rz "polyrez" builder
              | _ -> rz)
              |> return)
    | Aite (cond, { lets = then_lets; res = tr }, { lets = else_lets; res = fl })
      ->
        let curr_block = insertion_block builder in
        let fun_block = block_parent curr_block in
        let test_block = append_block contex "test" fun_block in
        let then_block = append_block contex "then" fun_block in
        let else_block = append_block contex "else" fun_block in
        let merge_block = append_block contex "merge" fun_block in

        ignore (build_br test_block builder);
        position_at_end test_block builder;
        let* cond = codegen_imm cond in
        let cond_val = build_icmp Icmp.Eq cond (int_const 0) "cond" builder in
        ignore (build_cond_br cond_val else_block then_block builder);

        position_at_end then_block builder;
        monadic_map (List.rev then_lets) codegen_local_var
        *>
        let* then_val = codegen_imm tr in
        let new_then_block = insertion_block builder in

        position_at_end else_block builder;
        monadic_map (List.rev else_lets) codegen_local_var
        *>
        let* else_val = codegen_imm fl in
        let new_else_block = insertion_block builder in

        let addr =
          insertion_block builder |> block_parent |> entry_block |> instr_begin
          |> builder_at contex
          |> build_alloca (type_of else_val) "ifrezptr"
        in
        position_at_end new_then_block builder;
        ignore (build_store then_val addr builder);
        ignore (build_br merge_block builder);

        position_at_end new_else_block builder;
        ignore (build_store else_val addr builder);
        ignore (build_br merge_block builder);

        position_at_end merge_block builder;
        build_load (type_of else_val) addr "ifrez" builder |> return
    | ATupleAccess (ImmVal name, ix) ->
        let n = str_name name in
        let* str = try_find n "Can't find tuple" in
        let t = typ name |> get_type in
        let addr =
          build_gep t str [| int_const 0; int_const ix |] "access" builder
        in
        build_load (Array.get (subtypes t) ix) addr "loadaccesss" builder
        |> return
    | _ -> error "never happen"

  and codegen_local_var { name; e } =
    let name = str_name name in
    let* body = codegen_expr e in
    let alloca = build_alloca (type_of body) (String.cat name "a") builder in
    ignore (build_store body alloca builder);
    let r = build_load (type_of body) alloca (String.cat name "l") builder in
    Hashtbl.add named_values name (Val r);
    return r

  let codegen_anf_binding main = function
    | AnfVal ({ name; e } as bind) -> (
        match typ name with
        | TyGround TUnit ->
            position_at_end main builder;
            codegen_expr e *> return ()
        | _ ->
            Hashtbl.add named_values (str_name name)
              (Func (bind, codegen_local_var));
            return ())
    | AnfFun { name; args; body = { lets; res = body } } -> (
        let* f = codegen_sig name args in
        let bb = append_block contex "entry" f in
        position_at_end bb builder;
        monadic_map (List.rev lets) codegen_local_var
        *>
        try
          let* ret_val = codegen_imm body in
          ignore (build_ret ret_val builder);
          return ()
        with _ ->
          delete_function f;
          error "Funciton binding error")

  let codegen_ret_main b =
    position_at_end b builder;
    build_ret (int_const 0) builder

  let codegen_main =
    let dec = function_type integer_type [||] in
    declare_function "main" dec the_module |> append_block contex "entry"

  let top_lvl code =
    let b = codegen_main in
    monadic_map (List.rev code) (codegen_anf_binding b)
    *>
    let _ = codegen_ret_main b in
    return ()

  let dmp_code file = print_module file the_module
end
