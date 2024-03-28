(** Copyright 2023-2024, Ilya Pankratov, Maxim Drumov *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open Anf
open LL_ast
open Ast
open Base
open Monads.VariableNameGeneratorMonad

(*
   Some thoughts about anf conversion.

   Let a top-level declaration be called the following:
   let f ... = ....
   let f = ...
   In this case f is a top-level declaration.

   Let the following constructs be called any level declaration:
   let f ... = ...
   let f = ...
   let ... =
   let f = ... in

   In all such cases, f is a declaration at any level.

   All top-level declarations without arguments are considered functions, which
   require 0 arguments to be passed to call them.

   Working with functions adheres to the following rules:
   1. If a top-level declaration with more than zero arguments is returned from
   any level declaration, that top-level declaration is turned into a closure to
   which zero arguments are applied.
   2. If a top-level declaration with zero arguments is returned from any level declaration,
   then that top-level declaration is considered a function to which zero arguments should be
   applied, and it is immediately called, and then we return the result of that application
   from the declaration.
   4. If the top-level declaration represents a higher-order function, it expects any function
   passed to it to be represented by a closure.
   3. If we pass a top-level declaration to a top-level declaration, the caller must
   turn the top-level declaration into a closure to which zero arguments are applied.
   4. If fewer arguments than expected are applied to the top-level declaration, then
   a closure is created from that top-level declaration to which the passed number of
   arguments is applied.
   5. If exactly as many arguments are applied to the top-level declaration as it expects,
   then an application of all arguments to the top-level declaration is called.
   6. If more arguments are applied to the top-level declaration than it expects, we first
   call the application with the arguments that the top-level declaration expects, and then
   apply the arguments to the result using AddArgsToClosure, since we expect the result to
   return a closure.
*)

(* Simply convert type from const to immexpr *)
let const_to_immexpr = function
  | CInt i -> ImmNum i
  | CBool b -> ImmBool b
;;

module Env : sig
  type args_env

  val add : args_env -> string -> int -> args_env
  val get_opt : args_env -> string -> int option
  val get : args_env -> string -> int
  val empty : args_env
end = struct
  module E = Stdlib.Map.Make (String)

  type args_env = int E.t

  let add env f n_arg = E.add f n_arg env
  let get_opt env f = E.find_opt f env

  let get env f =
    match get_opt env f with
    | Some x -> x
    | _ -> 0
  ;;

  let empty = E.empty
end

let is_imm_top_declaration env = function
  | ImmId var ->
    (match Env.get_opt env var with
     | Some args_n -> Some (var, args_n)
     | _ -> None)
  | _ -> None
;;

let get_lvar_name = function
  | LVar (name, _) -> Some name
  | _ -> None
;;

(*
   Converts llexpr to aexpr
   Argument expr_with_hole helps to create anf tree in cps
*)
let anf env e expr_with_hole =
  let rec helper ?applied_args (e : llexpr) (expr_with_hole : immexpr -> aexpr t) =
    match e with
    | LConst (const, _) -> expr_with_hole (const_to_immexpr const)
    | LVar (name, _) ->
      (match Env.get_opt env name with
       | Some 0 ->
         let* gl_name = fresh "global_var" in
         let gl_immid = ImmId gl_name in
         let immid = ImmId name in
         let* hole = expr_with_hole gl_immid in
         return (ALet (gl_name, CApp (immid, []), hole))
       | Some n when Option.is_none applied_args ->
         let* cl_name = fresh "empty_closure" in
         let cl_immid = ImmId cl_name in
         let immid = ImmId name in
         let* hole = expr_with_hole cl_immid in
         return (ALet (cl_name, CMakeClosure (immid, n, 0, []), hole))
       | _ -> expr_with_hole (ImmId name))
    | LBinop ((op, _), e1, e2) ->
      helper e1 (fun limm ->
        helper e2 (fun rimm ->
          let* new_name = fresh "binop" in
          let* hole = expr_with_hole @@ ImmId new_name in
          return (ALet (new_name, CBinOp (op, limm, rimm), hole))))
    | LApp _ as application ->
      let construct_app expr_with_hole imm args =
        let* new_name = fresh "app" in
        let* hole = expr_with_hole (ImmId new_name) in
        return (ALet (new_name, CApp (imm, args), hole))
      in
      let construct_closure expr_with_hole imm args =
        let* new_name = fresh "closure" in
        let* hole = expr_with_hole (ImmId new_name) in
        let _, n = Stdlib.Option.get @@ is_imm_top_declaration env imm in
        return (ALet (new_name, CMakeClosure (imm, n, List.length args, args), hole))
      in
      let construct_add_args_to_closure expr_with_hole imm args =
        let* new_name = fresh "closure" in
        let* hole = expr_with_hole (ImmId new_name) in
        return (ALet (new_name, CAddArgsToClosure (imm, List.length args, args), hole))
      in
      let construct_app_add_args_to_closure expr_with_hole imm app_args cl_args =
        let app = CApp (imm, app_args) in
        let* new_app = fresh "app" in
        let* new_closure = fresh "closure" in
        let* hole = expr_with_hole (ImmId new_closure) in
        return
          (ALet
             ( new_app
             , app
             , ALet
                 ( new_closure
                 , CAddArgsToClosure (ImmId new_app, List.length cl_args, cl_args)
                 , hole ) ))
      in
      (* app_helper is used for collecting all arguments at the application *)
      let rec app_helper curr_args = function
        | LApp (a, b, _) -> helper b (fun imm -> app_helper (imm :: curr_args) a)
        | f ->
          helper ~applied_args:(List.length curr_args) f (fun imm ->
            match is_imm_top_declaration env imm with
            | None ->
              (* Not top-level declaration. Expect that it's closure *)
              construct_add_args_to_closure expr_with_hole imm curr_args
            | Some (_, 0) ->
              (* It's top level declaration, that takes zero arguments.
                 We expect that the top level declaration is closure. *)
              construct_add_args_to_closure expr_with_hole imm curr_args
            | Some (_, n) ->
              if n = List.length curr_args
              then (
                (* If we can apply all arguments to a top level declaration, we do it. *)
                match get_lvar_name f with
                | Some x -> construct_app expr_with_hole (ImmId x) curr_args
                | _ -> construct_app expr_with_hole imm curr_args)
              else if List.length curr_args < n
              then
                (* Top level declaration need more arguments. Create a closure for now.*)
                construct_closure expr_with_hole imm curr_args
              else (
                let app_args, closure_args = List.split_n curr_args n in
                (* Apply all arguments that we can to the top level declarations.
                   We expect that return value will be closure. So then we add all arguments to the closure.
                *)
                construct_app_add_args_to_closure expr_with_hole imm app_args closure_args))
      in
      app_helper [] application
    | LLetIn ((name, _), e1, e2) ->
      helper e1 (fun immval ->
        let* aexpr = helper e2 expr_with_hole in
        return (ALet (name, CImmExpr immval, aexpr)))
    | LIfThenElse (i, t, e, _) ->
      helper i (fun immif ->
        let* athen = helper t (fun immthen -> return @@ ACEexpr (CImmExpr immthen)) in
        let* aelse = helper e (fun immelse -> return @@ ACEexpr (CImmExpr immelse)) in
        let* new_name = fresh "if" in
        let* hole = expr_with_hole @@ ImmId new_name in
        return @@ ALet (new_name, CIfThenElse (immif, athen, aelse), hole))
    | LTuple (elems, _) ->
      let* new_name = fresh "tuple" in
      let rec tuple_helper l = function
        | hd :: tl -> helper hd (fun imm -> tuple_helper (imm :: l) tl)
        | _ ->
          let* hole = expr_with_hole (ImmId new_name) in
          return (ALet (new_name, CTuple (List.rev l), hole))
      in
      tuple_helper [] elems
    | LTake (lexpr, n) ->
      helper lexpr (fun imm ->
        let* new_name = fresh "take" in
        let* hole = expr_with_hole (ImmId new_name) in
        return (ALet (new_name, CTake (imm, n), hole)))
  in
  helper e expr_with_hole
;;

(* Performs transformation from llbinding to anfexpr *)
let anf_binding env = function
  | LLet ((name, _), args, expr) | LLetRec ((name, _), args, expr) ->
    let constructor name args aexpr = AnfLetFun (name, args, aexpr) in
    let args =
      List.map
        ~f:(function
          | Used (s, _) -> Anf.Used s
          | _ -> Anf.Unused)
        args
    in
    let env = Env.add env name (List.length args) in
    let* aexpr = anf env expr (fun imm -> return (ACEexpr (CImmExpr imm))) in
    return (env, constructor name args aexpr)
;;

(* Performs transformation from Toplevel.llstatements to Anf.anfstatements *)
let anf lstatements =
  let env =
    List.fold
      ~init:Env.empty
      ~f:(fun acc (name, _, n) -> Env.add acc name n)
      Jamlstdlib.stdlib_functions
  in
  List.rev
  @@ snd
  @@ run
  @@ monad_fold
       ~init:(env, [])
       ~f:(fun (env, stmts) lbinding ->
         let* env, stmt = anf_binding env lbinding in
         return (env, stmt :: stmts))
       lstatements
;;
