(** Copyright 2023-2024, Grigory Aseev and Matvey Kalashnikov *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open RestrictedAst
open Llast
open Ast
open AnfPrinter
open Counter

(** Converts constant to immediate value *)
let conv_const = function
  | CBool b -> ImmBool b
  | CInt i -> ImmNum i
  | CUnit -> ImmUnit
;;

(** Converts pattern to immediate pattern *)
let conv_pattern = function
  | PWild -> PImmWild
  | PVar id -> PImmExpr (ImmId id)
  | PConst c -> PImmExpr (conv_const c)
;;

open IState
open IState.Syntax

(** Variable Generation *)
let rec gen_var base global =
  let* fresh_id = fresh_name_int in
  let new_id = base ^ Int.to_string fresh_id in
  let is_in = Base.Set.mem global new_id in
  if is_in then gen_var base global else return new_id
;;

let rec anf_expr
  (env : (string, 'a) Base.Set.t)
  (e : llexpr)
  (expr_with_imm_hole : immexpr -> aexpr t)
  : aexpr t
  =
  match e with
  | LLConst c -> expr_with_imm_hole @@ conv_const c
  | LLVar id -> expr_with_imm_hole @@ ImmId id
  | LLBinOp (op, left, right) ->
    anf_expr env left (fun limm ->
      anf_expr env right (fun rimm ->
        let* var = gen_var "b_op_" env in
        expr_with_imm_hole @@ ImmId var
        >>= fun ae -> return (ALetIn (var, CBinOp (op, limm, rimm), ae))))
  | LLIf (cond, t, e) ->
    anf_expr env cond (fun cimm ->
      let* var = gen_var "if_" env in
      expr_with_imm_hole @@ ImmId var
      >>= fun ae ->
      let* taexpr = anf_expr env t (fun timm -> return @@ ACExpr (CImmExpr timm)) in
      let* eaexpr = anf_expr env e (fun eimm -> return @@ ACExpr (CImmExpr eimm)) in
      return (ALetIn (var, CIf (cimm, taexpr, eaexpr), ae)))
  | LLApp (f, arg) ->
    anf_expr env f (fun fimm ->
      anf_expr env arg (fun argimm ->
        let* var = gen_var "app_" env in
        expr_with_imm_hole @@ ImmId var
        >>= fun ae -> return (ALetIn (var, CApp (fimm, argimm), ae))))
  | LLLetIn (varname, e1, e2) ->
    let new_env = Base.Set.add env varname in
    anf_expr new_env e1 (fun immval ->
      anf_expr new_env e2 expr_with_imm_hole
      >>= fun body -> return (ALetIn (varname, CImmExpr immval, body)))
;;

let anf_binding env = function
  | LLLet (r, varname, args, e) ->
    let rec get_initial_env args acc =
      match args with
      | [] -> acc
      | PVar id :: tl -> Base.Set.add acc id |> get_initial_env tl
      | _ :: tl -> get_initial_env tl acc
    in
    anf_expr (get_initial_env args env) e (fun ie -> return (ACExpr (CImmExpr ie)))
    >>= fun anf_e -> return (ALet (r, varname, List.map conv_pattern args, anf_e))
;;

let anf_program (binds : llbinding list) =
  let rec get_initial_env binds acc =
    match binds with
    | [] -> acc
    | LLLet (_, varname, _, _) :: tl -> Base.Set.add acc varname |> get_initial_env tl
  in
  let env = get_initial_env binds (Base.Set.empty (module Base.String)) in
  List.map (fun bind -> snd @@ IState.runState ~init:0 (anf_binding env bind)) binds
;;

let print_anf_prog llbinds =
  let res = anf_program llbinds in
  Format.printf "%a" pp_prexpr res
;;

let print_anf_expr llexpr =
  let res =
    snd
    @@ IState.runState
         ~init:0
         (anf_expr
            (Base.Set.empty (module Base.String))
            llexpr
            (fun ie -> return (ACExpr (CImmExpr ie))))
  in
  Format.printf "%a" pp_aexpr res
;;

let%expect_test _ =
  print_anf_expr
  @@ LLBinOp (Sub, LLBinOp (Add, LLConst (CInt 5), LLConst (CInt 4)), LLConst (CInt 2));
  [%expect {|
    let b_op_0 = 5 + 4 in
     let b_op_1 = b_op_0 - 2 in
     b_op_1 |}]
;;

let%expect_test _ =
  print_anf_expr
  @@ LLBinOp
       ( Add
       , LLBinOp (Add, LLConst (CInt 5), LLBinOp (Sub, LLConst (CInt 4), LLConst (CInt 3)))
       , LLConst (CInt 2) );
  [%expect
    {|
    let b_op_0 = 4 - 3 in
     let b_op_1 = 5 + b_op_0 in
     let b_op_2 = b_op_1 + 2 in
     b_op_2 |}]
;;

let%expect_test _ =
  print_anf_expr
  @@ LLBinOp
       ( Add
       , LLBinOp (Add, LLConst (CInt 5), LLConst (CInt 4))
       , LLBinOp (Add, LLConst (CInt 3), LLConst (CInt 2)) );
  [%expect
    {|
    let b_op_0 = 5 + 4 in
     let b_op_1 = 3 + 2 in
     let b_op_2 = b_op_0 + b_op_1 in
     b_op_2 |}]
;;

let%expect_test _ =
  (*
     let fack1 k n m = k (n * m);;
     let rec fack n k = if n <= 1 then k 1 else fack (n-1) (fack1 k n);;
     let id x = x;;
     let fac n = fack n id
  *)
  print_anf_prog
  @@ [ LLLet
         ( false
         , "fack1"
         , [ PVar "k"; PVar "n"; PVar "m" ]
         , LLApp (LLVar "k", LLBinOp (Mul, LLVar "n", LLVar "m")) )
     ; LLLet
         ( true
         , "fack"
         , [ PVar "n"; PVar "k" ]
         , LLIf
             ( LLBinOp (Leq, LLVar "n", LLConst (CInt 1))
             , LLApp (LLVar "k", LLConst (CInt 1))
             , LLApp
                 ( LLApp (LLVar "fack", LLBinOp (Sub, LLVar "n", LLConst (CInt 1)))
                 , LLApp (LLApp (LLVar "fack1", LLVar "k"), LLVar "n") ) ) )
     ; LLLet (false, "id", [ PVar "x" ], LLVar "x")
     ; LLLet
         (false, "fac", [ PVar "n" ], LLApp (LLApp (LLVar "fack", LLVar "n"), LLVar "id"))
     ];
  [%expect
    {|
    let fack1 k n m = let b_op_0 = n * m in
     let app_1 = k b_op_0 in
     app_1;;
    let rec fack n k = let b_op_0 = n <= 1 in
     let if_1 = if b_op_0 then let app_2 = k 1 in
     app_2 else let b_op_3 = n - 1 in
     let app_4 = fack b_op_3 in
     let app_5 = fack1 k in
     let app_6 = app_5 n in
     let app_7 = app_4 app_6 in
     app_7 in
     if_1;;
    let id x = x;;
    let fac n = let app_0 = fack n in
     let app_1 = app_0 id in
     app_1 |}]
;;

let%expect_test _ =
  (*
     let id x = x
     let acc1 acc x y = acc (x + y)
     let acc2 fib_func n acc x = fib_func (n - 2) (acc1 acc x)
     let rec fibo_cps n acc = if n < 3 then acc 1 else fibo_cps (n - 1) (acc2 fibo_cps n acc)
     let fibo n = fibo_cps n id
  *)
  print_anf_prog
  @@ [ LLLet (false, "id", [ PVar "x" ], LLVar "x")
     ; LLLet
         ( false
         , "acc1"
         , [ PVar "acc"; PVar "x"; PVar "y" ]
         , LLApp (LLVar "acc", LLBinOp (Add, LLVar "x", LLVar "y")) )
     ; LLLet
         ( false
         , "acc2"
         , [ PVar "fib_func"; PVar "n"; PVar "acc"; PVar "x" ]
         , LLApp
             ( LLApp (LLVar "fib_func", LLBinOp (Sub, LLVar "n", LLConst (CInt 2)))
             , LLApp (LLApp (LLVar "acc1", LLVar "acc"), LLVar "x") ) )
     ; LLLet
         ( true
         , "fibo_cps"
         , [ PVar "n"; PVar "acc" ]
         , LLIf
             ( LLBinOp (Less, LLVar "n", LLConst (CInt 3))
             , LLApp (LLVar "acc", LLConst (CInt 1))
             , LLApp
                 ( LLApp (LLVar "fibo_cps", LLBinOp (Sub, LLVar "n", LLConst (CInt 1)))
                 , LLApp
                     ( LLApp (LLApp (LLVar "acc2", LLVar "fibo_cps"), LLVar "n")
                     , LLVar "acc" ) ) ) )
     ; LLLet
         ( false
         , "fibo"
         , [ PVar "n" ]
         , LLApp (LLApp (LLVar "fibo_cps", LLVar "n"), LLVar "id") )
     ];
  [%expect
    {|
    let id x = x;;
    let acc1 acc x y = let b_op_0 = x + y in
     let app_1 = acc b_op_0 in
     app_1;;
    let acc2 fib_func n acc x = let b_op_0 = n - 2 in
     let app_1 = fib_func b_op_0 in
     let app_2 = acc1 acc in
     let app_3 = app_2 x in
     let app_4 = app_1 app_3 in
     app_4;;
    let rec fibo_cps n acc = let b_op_0 = n < 3 in
     let if_1 = if b_op_0 then let app_2 = acc 1 in
     app_2 else let b_op_3 = n - 1 in
     let app_4 = fibo_cps b_op_3 in
     let app_5 = acc2 fibo_cps in
     let app_6 = app_5 n in
     let app_7 = app_6 acc in
     let app_8 = app_4 app_7 in
     app_8 in
     if_1;;
    let fibo n = let app_0 = fibo_cps n in
     let app_1 = app_0 id in
     app_1 |}]
;;
