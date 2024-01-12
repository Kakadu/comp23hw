(** Copyright 2023-2024, Grigory Aseev and Matvey Kalashnikov *)

(** SPDX-License-Identifier: LGPL-3.0 *)

open Ast
open Format

let pp_const fmt = function
  | CInt x -> fprintf fmt "%d" x
  | CBool x -> fprintf fmt "%b" x
  | CUnit -> fprintf fmt "()"
;;

let pp_bin_op fmt = function
  | Add -> fprintf fmt "+"
  | Sub -> fprintf fmt "-"
  | Mul -> fprintf fmt "*"
  | Div -> fprintf fmt "/"
  | Eq -> fprintf fmt "="
  | Neq -> fprintf fmt "<>"
  | Less -> fprintf fmt "<"
  | Leq -> fprintf fmt "<="
  | Gre -> fprintf fmt ">"
  | Geq -> fprintf fmt ">="
  | And -> fprintf fmt "&&"
  | Or -> fprintf fmt "||"
  | Mod -> fprintf fmt "%%"
;;

let pp_pattern fmt = function
  | PVar x -> fprintf fmt "%s" x
  | PConst c -> pp_const fmt c
  | PWild -> fprintf fmt "_"
;;

let rec pp_patterns fmt = function
  | [] -> fprintf fmt ""
  | p :: tl -> fprintf fmt "%a %a" pp_pattern p pp_patterns tl
;;

let pp_rec_flag fmt = function
  | true -> fprintf fmt "rec"
  | false -> fprintf fmt ""
;;

let rec eletin_helper fmt = function
  | EFun (p, e) -> fprintf fmt "%a %a" pp_pattern p eletin_helper e
  | _ -> fprintf fmt ""
;;

let rec pp_expr fmt = function
  | EConst c -> pp_const fmt c
  | EBinOp (op, e1, e2) -> fprintf fmt "(%a %a %a)" pp_expr e1 pp_bin_op op pp_expr e2
  | EVar x -> fprintf fmt "%s" x
  | EIf (e1, e2, e3) ->
    fprintf fmt "if %a then %a else %a" pp_expr e1 pp_expr e2 pp_expr e3
  | EFun (p, e) -> fprintf fmt "(fun %a -> %a)" pp_pattern p pp_expr e
  | ELetIn (rec_flag, x, e1, e2) ->
    fprintf
      fmt
      "let %a %s %a = %a in %a"
      pp_rec_flag
      rec_flag
      x
      eletin_helper
      e1
      efun_helper
      e1
      pp_expr
      e2
  | EApp (e1, e2) -> fprintf fmt "%a %a" pp_expr e1 pp_expr e2

and efun_helper fmt = function
  | EFun (_, e) -> fprintf fmt "%a" efun_helper e
  | other -> fprintf fmt "%a" pp_expr other

and pp_binding fmt = function
  | ELet (rec_flag, x, e) ->
    fprintf fmt "let %a %s %a = %a" pp_rec_flag rec_flag x eletin_helper e efun_helper e
;;

let%expect_test _ =
  printf "%a" pp_binding
  @@ ELet
       ( false
       , "fac"
       , EFun
           ( PVar "n"
           , ELetIn
               ( true
               , "fack"
               , EFun
                   ( PVar "n"
                   , EFun
                       ( PVar "k"
                       , EIf
                           ( EBinOp (Leq, EVar "n", EConst (CInt 1))
                           , EApp (EVar "k", EConst (CInt 1))
                           , EApp
                               ( EApp
                                   (EVar "fack", EBinOp (Sub, EVar "n", EConst (CInt 1)))
                               , EFun
                                   ( PVar "m"
                                   , EApp (EVar "k", EBinOp (Mul, EVar "m", EVar "n")) )
                               ) ) ) )
               , EApp (EApp (EVar "fack", EVar "n"), EFun (PVar "x", EVar "x")) ) ) );
  [%expect
    {| let  fac n  = let rec fack n k  = if (n <= 1) then k 1 else fack (n - 1) (fun m -> k (m * n)) in fack n (fun x -> x) |}]
;;
