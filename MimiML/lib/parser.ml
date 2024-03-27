(** Copyright 2023, Lev Golofastov & Ksenia Kuzmina *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Angstrom
open Ast

(* Mini-parsers *)

(** skip spaces *)
let spaces = take_while (fun c -> c = ' ' || c = '\n' || c = '\r')

(** skip spaces and newlines *)
let spaces_nls = take_while (fun c -> c = ' ' || c = '\n')

(** trim spaces before p and after *)
let trim p = spaces *> p <* spaces

let trim_nls p = spaces_nls *> p <* spaces_nls

(** trim parens around p **)
let parens p = char '(' *> p <* char ')'

let is_digit = function
  | '0' .. '9' -> true
  | _ -> false
;;

let is_letter = function
  | 'a' .. 'z' | 'A' .. 'Z' | '_' -> true
  | _ -> false
;;

(** parser integer literal *)
let int_p = take_while1 is_digit >>| int_of_string

(** parses bool literal *)
let bool_p = string "true" <|> string "false" >>| ( = ) "true"

(** parses unit *)

let unit_p = parens (return ())

(** keyword predicate *)
let is_kw = function
  | "if" | "let" | "in" | "else" | "fun" | "function" | "then" -> true
  | _ -> false
;;

(** string literal parser *)
let string_p = char '"' *> take_while (( != ) '"') <* char '"'

(** variable name parser *)
let varname_p =
  take_while1 (fun c -> is_letter c || is_digit c)
  >>= function
  | word when is_digit (String.get word 0) -> fail "Varname cannot start by digit"
  | word when is_kw word -> fail "Found keyword"
  | word -> return word
;;

let pattern = spaces *> varname_p >>| patvar

(* Original code was taken from
   https://gitlab.com/Kakadu/fp2020course-materials/-/blob/master/code/miniml/parsing.ml,
   but has been rewritten to be more readable *)

type dispatch =
  { priors : dispatch -> expr t
  ; e : dispatch -> expr t
  ; app : dispatch -> expr t
  }

(** check if we support this operator *)
let is_op = function
  | "+" | "-" | "*" | "/" | "=" | "^" | "<" | ">" | "<=" | ">=" -> true
  | _ -> false
;;

let both = lift2 (fun x y -> x, y)

let priors e =
  let pair s = return (fun a -> eapp (eapp (evar s) a)), spaces *> string s in
  let table =
    [| [ pair "="; pair "<"; pair ">"; pair ">="; pair "<=" ]
     ; [ pair "^" ]
     ; [ pair "+"; pair "-" ]
     ; [ pair "*"; pair "/" ]
    |]
  in
  let len = Array.length table in
  let rec helper = function
    | lvl when lvl >= len -> e
    | lvl ->
      let next = helper (lvl + 1) in
      lift2
        (List.fold_left (fun acc (op, r) -> op acc r))
        next
        (many (choice (List.map (fun (f, op) -> both f (op *> next)) table.(lvl))))
  in
  helper 0
;;

let let_p e =
  lift4
    (fun rec_f name ps rhs -> rec_f, name, List.fold_right elam ps rhs)
    (string "let" *> option nrecf (trim (string "rec" *> return recf)))
    pattern
    (many pattern)
    (trim (char '=') *> e)
;;

let const_p = take_while1 (fun c -> c != '%' && c != '"')

(** main function, that return dispatch (dispatch contains all main parsers) *)
let pack =
  let priors d = priors (d.app d) in
  let c_expr_p =
    choice
      [ int_p >>| cint; bool_p >>| cbool; string_p >>| cstring; unit_p *> return cunit ]
    >>| econst
  in
  let op =
    take 1
    >>= function
    | str when is_op str -> return (evar str)
    | _ -> fail "Expected op in parents"
  in
  let evar_p = varname_p >>| evar in
  let fun_p d =
    lift2
      (List.fold_right elam)
      (string "fun" *> many1 (trim pattern))
      (trim (string "->") *> d.priors d)
  in
  let cond_p d =
    lift3
      eifelse
      (string "if" *> d.priors d)
      (string "then" *> d.priors d)
      (string "else" *> d.priors d)
  in
  let in_part_p d = trim (string "in") *> spaces_nls *> d.priors d in
  let let_in_p d = lift2 elet (let_p (d.priors d)) (in_part_p d) in
  (* parses expr *)
  let e d =
    let exprs_list d =
      let in_parens = char '(' *> (d.priors d <|> trim op) <* char ')' in
      [ in_parens; trim c_expr_p; trim evar_p; fun_p d; cond_p d; let_in_p d ]
    in
    fix (fun _ -> spaces *> choice (exprs_list d))
  in
  let app d = lift2 (List.fold_left eapp) (d.e d) (many (d.e d)) in
  { e; app; priors }
;;

let parse_pack p = parse_string ~consume:All (trim_nls (p pack))
let parse = parse_pack pack.priors

let test ?(pp = true) str =
  match parse str with
  | Ok res -> (if pp then Pp.pp_expr else pp_expr) Format.std_formatter res
  | Error err -> print_endline err
;;

let%expect_test "Integer" =
  test "1";
  [%expect {| 1 |}]
;;

let%expect_test "Arithmetic" =
  test "1+2*4/6";
  [%expect {|
    (((+) 1) (((/) (((*) 2) 4)) 6)) |}]
;;

let%expect_test "Using operator in parens" =
  test "(((+) 1) (((/) (((*) 2) 4)) 6))";
  [%expect {|
    (((+) 1) (((/) (((*) 2) 4)) 6)) |}]
;;

let%expect_test "Simple operator in parens" =
  test "(+) 1 2";
  [%expect {| (((+) 1) 2) |}]
;;

let%expect_test "Nested operator in parens" =
  test "(+) 1 ((-) 1 2)";
  [%expect {| (((+) 1) (((-) 1) 2)) |}]
;;

let%expect_test "Identity fun" =
  test "(fun x -> x)";
  [%expect {|(fun x -> x)|}]
;;

let%expect_test "Nested fun" =
  test "(fun x -> (fun y -> 2))";
  [%expect {|(fun x -> (fun y -> 2))|}]
;;

let%expect_test "Nested fun" =
  test "(fun x z -> (fun y -> z))";
  [%expect {|(fun x -> (fun z -> (fun y -> z)))|}]
;;

let%expect_test "Let in" =
  test "let x = y in fun y -> x";
  [%expect {|let x = y in (fun y -> x)|}]
;;

let%expect_test "Let rec in" =
  test "let rec f = fun n -> f in f";
  [%expect {|let rec f = (fun n -> f) in f|}]
;;

let%expect_test "Let rec in" =
  test "if y then x else y";
  [%expect {|if y then x else y|}]
;;

let%expect_test "Let in" =
  test ~pp:false "(+) 1";
  [%expect {|(EApp ((EVar "+"), (EConst (CInt 1))))|}]
;;

let%expect_test "Let in" =
  test ~pp:false "(+) 1 2";
  [%expect {|(EApp ((EApp ((EVar "+"), (EConst (CInt 1)))), (EConst (CInt 2))))|}]
;;

let%expect_test "Let in" =
  test "let rec x = 5 in x";
  [%expect {|let rec x = 5 in x|}]
;;

let%expect_test "Concat" =
  test {|"fp" ^ "2022"|};
  [%expect {| (((^) "fp") "2022") |}]
;;

let%expect_test _ =
  test {|f x y z * 5|};
  [%expect {| (((*) (((f x) y) z)) 5) |}]
;;

let%expect_test _ =
  test ~pp:false {|(+) 5|};
  [%expect {| (EApp ((EVar "+"), (EConst (CInt 5)))) |}]
;;

let%expect_test _ =
  test ~pp:false {|(+) 5 6|};
  [%expect {| (EApp ((EApp ((EVar "+"), (EConst (CInt 5)))), (EConst (CInt 6)))) |}]
;;
