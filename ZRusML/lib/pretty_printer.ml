(** Copyright 2023-2024, Rustam Shangareev and Danil Yevdokimov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

open Ast
open Format

let show_const = function
  | CInt x -> sprintf "%d" x
  | CBool x -> sprintf "%b" x
;;

let pp_const fmt cnst = fprintf fmt "%s" (show_const cnst)

let show_bin_op = function
  | And -> "&&"
  | Or -> "||"
  | Less -> "<"
  | Leq -> "<="
  | Gre -> ">"
  | Geq -> ">="
  | Eq -> "="
  | Neq -> "<>"
  | Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Div -> "/"
;;

let pp_bin_op fmt op = fprintf fmt "%s" (show_bin_op op)

let show_un_op = function
  | Not -> "not"
  | Minus -> "-"
;;

let pp_un_op fmt op = fprintf fmt "%s" (show_un_op op)

let show_pt = function
  | PtWild -> "_"
  | PtVar x -> sprintf "%s" x
  | PtConst x -> sprintf "%s" (show_const x)
;;

let pp_pt fmt pt = fprintf fmt "%s" (show_pt pt)
let sprintf_tabs cnt = sprintf "%s" (String.concat "" (List.init cnt (fun _ -> "    ")))
let print_tabs fmt cnt = fprintf fmt "%s" (sprintf_tabs cnt)

let rec show_exp_helper cnt = function
  | EConst x -> sprintf "%s" (show_const x)
  | EUnOp (o, e) -> sprintf "%s%s" (show_un_op o) (show_exp_helper cnt e)
  | EVar id -> sprintf "%s" id
  | EIf (predicate, true_branch, false_branch) ->
    sprintf
      "if %s then %s else %s"
      (show_exp_helper cnt predicate)
      (show_exp_helper cnt true_branch)
      (show_exp_helper cnt false_branch)
  | EBinOp (op, e1, e2) ->
    let s, e =
      match op with
      | Sub | Add | Mul | Eq | Geq | Leq | Gre | Less -> "(", ")"
      | _ -> "", ""
    in
    sprintf
      "%s%s %s %s%s"
      s
      (show_exp_helper cnt e1)
      (show_bin_op op)
      (show_exp_helper cnt e2)
      e
  | EFun _ as orig ->
    let rec helper = function
      | EFun (a, b) -> sprintf "%s %s" (show_pt a) (helper b)
      | exp -> sprintf "-> %s" (show_exp_helper cnt exp)
    in
    sprintf "(fun %s)" (helper orig)
  | ELet (bindings, e) ->
    let cnt = cnt + 1 in
    let binds =
      List.fold_left
        (fun acc (is_rec, pt, exp) ->
          let rec helper = function
            | EFun (a, b) -> sprintf "%s %s" (show_pt a) (helper b)
            | exp -> sprintf "= %s" (show_exp_helper cnt exp)
          in
          let bind =
            sprintf
              "%slet %s%s %s in\n"
              (sprintf_tabs cnt)
              (if is_rec then "rec " else "")
              (show_pt pt)
              (helper exp)
          in
          let acc = acc ^ bind in
          acc)
        String.empty
        bindings
    in
    sprintf
      "\n%s%s%s\n%s"
      binds
      (sprintf_tabs cnt)
      (show_exp_helper cnt e)
      (sprintf_tabs (cnt - 1))
  | EApp (e1, e2) ->
    let s, e =
      match e2 with
      | EApp _ -> "(", ")"
      | _ -> "", ""
    in
    sprintf "%s %s%s%s" (show_exp_helper cnt e1) s (show_exp_helper cnt e2) e
;;

let show_exp exp = sprintf "%s" (show_exp_helper 0 exp)
let pp_exp fmt exp = fprintf fmt "%s" (show_exp exp)

let show_binding_helper cnt (is_rec, pt, exp) =
  let rec helper = function
    | EFun (a, b) -> sprintf "%s %s" (show_pt a) (helper b)
    | e -> sprintf "= %s" (show_exp_helper cnt e)
  in
  sprintf
    "%slet %s%s %s"
    (sprintf_tabs cnt)
    (if is_rec then "rec " else "")
    (show_pt pt)
    (helper exp)
;;

let show_binding = show_binding_helper 0
let pp_binding_helper fmt cnt x = fprintf fmt "%s" (show_binding_helper cnt x)
let pp_binding fmt = pp_binding_helper fmt 0
let show_decl (DLet t) = sprintf "%s;;\n" (show_binding t)
let pp_decl fmt x = fprintf fmt "%s" (show_decl x)
let show_prog = List.fold_left (fun acc decl -> acc ^ show_decl decl) ""
let pp_prog fmt prog = fprintf fmt "%s" (show_prog prog)
