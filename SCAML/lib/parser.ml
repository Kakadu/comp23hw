(** Copyright 2023-2024, Grigory Aseev and Matvey Kalashnikov *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open Angstrom
open Ast
open Base
module Format = Stdlib.Format

let parse_str p s = parse_string ~consume:All p s

let is_whitespace = function
  | ' ' | '\n' | '\t' | '\r' -> true
  | _ -> false
;;

let is_digit = function
  | '0' .. '9' -> true
  | _ -> false
;;

let is_aletter = function
  | 'a' .. 'z' -> true
  | _ -> false
;;

let is_cletter = function
  | 'A' .. 'Z' -> true
  | _ -> false
;;

let is_underscore = function
  | c -> Char.equal c '_'
;;

let is_id_char = function
  | c -> is_digit c || is_aletter c || is_cletter c || is_underscore c
;;

let is_keyword = function
  | "let" | "rec" | "fun" | "in" | "if" | "then" | "else" | "true" | "false" -> true
  | _ -> false
;;

let chainl1 e op =
  let rec go acc = lift2 (fun f x -> f acc x) op e >>= go <|> return acc in
  e >>= fun init -> go init
;;

let empty = take_while is_whitespace
let empty1 = take_while1 is_whitespace
let token s = empty *> s
let token1 s = empty1 *> s
let str_token s = empty *> string s
let str_token1 s = empty1 *> string s
let pparens p = str_token "(" *> p <* str_token ")"
let parrow = str_token "->"
let pbinding = str_token "let"
let pwild = str_token "_"

(**  Const parsers *)
let pcint =
  let ps = token (option "" (str_token "-" <|> str_token "+")) in
  let pd = take_while1 is_digit in
  lift2 (fun sign digit -> constr_cint (Int.of_string @@ sign ^ digit)) ps pd
;;

let pcbool =
  lift (fun b -> constr_cbool @@ Bool.of_string b) (str_token "false" <|> str_token "true")
;;

let pcunit = pparens @@ (str_token "" *> return CUnit)
let pconst = token (choice [ pcint; pcbool; pcunit ])

(**  Identifier parsers *)
let ident is_good_entry =
  let pchar = satisfy is_id_char in
  empty *> satisfy is_good_entry
  >>= fun h ->
  many pchar
  >>= fun tl ->
  let id = String.of_char_list (h :: tl) in
  if is_keyword id
  then fail ("Unexpected keyword '" ^ id ^ "' in binding")
  else if String.equal id "_"
  then fail "Wildcard \"_\" not expected"
  else return id
;;

let pident =
  let is_entry = function
    | c -> is_aletter c || is_underscore c
  in
  ident is_entry
;;

(**  Pattern parsers *)

let ppwild = constr_pwild <$> pwild
let ppconst = constr_pconst <$> pconst
let ppvar = constr_pvar <$> pident
let pattern = fix @@ fun m -> choice [ ppwild; ppconst; ppvar ] <|> pparens m

(**  Operation parsers *)

let pop ch op = str_token ch *> return (constr_ebinop op)
let pmulti = choice [ pop "*" Mul; pop "/" Div; pop "%" Mod ]
let padd = pop "+" Add <|> pop "-" Sub
let pcomp = choice [ pop ">=" Geq; pop ">" Gre; pop "<=" Leq; pop "<" Less ]
let peq = pop "=" Eq <|> pop "<>" Neq
let pconj = pop "&&" And
let pdisj = pop "||" Or

(** Expr parsers *)

type edispatch =
  { evar : edispatch -> expr t
  ; econst : edispatch -> expr t
  ; op : edispatch -> expr t
  ; condition : edispatch -> expr t
  ; func : edispatch -> expr t
  ; bind_in : edispatch -> expr t
  ; app : edispatch -> expr t
  ; expr : edispatch -> expr t
  }

let peconst = constr_econst <$> pconst
let pevar = pident >>| constr_evar
let pfun_args = fix @@ fun p -> many pattern <|> pparens p
let pfun_args1 = fix @@ fun p -> many1 pattern <|> pparens p
let pparens_only ps = pparens @@ choice ps

let plet_body pargs pexpr =
  token1 pargs
  >>= fun args -> str_token "=" *> pexpr >>| fun expr -> constr_efun args expr
;;

let plet_body_without_args pexpr =
  str_token "=" *> pexpr >>| fun expr -> constr_efun [] expr
;;

let pbind_with_option_rec = pbinding *> option false (str_token1 "rec " >>| fun _ -> true)
let pname_func = pident >>| fun name -> name
let pbody_with_args pack = plet_body pfun_args (pack.expr pack)

let pack =
  let expr pack =
    choice
      [ pack.op pack
      ; pack.app pack
      ; pack.condition pack
      ; pack.func pack
      ; pack.bind_in pack
      ]
  in
  let econst pack = fix @@ fun _ -> peconst <|> pparens @@ pack.econst pack in
  let evar pack = fix @@ fun _ -> pevar <|> pparens @@ pack.evar pack in
  let op_parsers pack =
    choice
      [ pack.bind_in pack
      ; pack.app pack
      ; pparens_only [ pack.op pack; pack.condition pack ]
      ; pack.evar pack
      ; pack.econst pack
      ]
  in
  let cond_bool_parsers pack =
    choice [ pack.op pack; pack.bind_in pack; pack.app pack; pack.condition pack ]
  in
  let app_func_parsers pack =
    choice
      [ pack.evar pack
      ; pparens_only
          [ pack.condition pack; pack.func pack; pack.app pack; pack.bind_in pack ]
      ]
  in
  let app_args_parsers pack =
    choice
      [ pparens_only
          [ pack.op pack
          ; pack.condition pack
          ; pack.func pack
          ; pack.app pack
          ; pack.bind_in pack
          ]
      ; pack.evar pack
      ; pack.econst pack
      ]
  in
  let op pack =
    fix
    @@ fun _ ->
    let multi = chainl1 (op_parsers pack) pmulti in
    let add = chainl1 multi padd in
    let comp = chainl1 add pcomp in
    let eq = chainl1 comp peq in
    let conj = chainl1 eq pconj in
    chainl1 conj pdisj <|> pparens @@ pack.op pack
  in
  let condition pack =
    fix
    @@ fun _ ->
    lift3
      constr_eif
      (str_token "if" *> cond_bool_parsers pack)
      (str_token1 "then" *> expr pack)
      (str_token1 "else" *> expr pack)
    <|> pparens @@ pack.condition pack
  in
  let func pack =
    fix
    @@ fun _ ->
    lift2 constr_efun (str_token "fun" *> pfun_args1) (parrow *> expr pack)
    <|> pparens @@ pack.func pack
  in
  let app pack =
    fix
    @@ fun _ ->
    lift2 constr_eapp (app_func_parsers pack) (many1 (token1 @@ app_args_parsers pack))
    <|> pparens @@ pack.app pack
  in
  let pbind_without_rec = pbinding *> return false in
  let punderscore_name = str_token1 "_" *> return "_" in
  let pbody_without_args pack = plet_body_without_args (expr pack) in
  let bind_in pack =
    fix
    @@ fun _ ->
    lift4
      constr_eletin
      pbind_with_option_rec
      pname_func
      (pbody_with_args pack)
      (str_token1 "in" *> expr pack)
    <|> lift4
          constr_eletin
          pbind_without_rec
          punderscore_name
          (pbody_without_args pack)
          (str_token1 "in" *> expr pack)
    <|> pparens @@ pack.bind_in pack
  in
  { evar; econst; op; condition; func; bind_in; app; expr }
;;

let expr = pack.expr pack

(**  Binding parser *)
let bind =
  fix
  @@ fun m ->
  lift3 constr_elet pbind_with_option_rec pname_func (pbody_with_args pack) <|> pparens m
;;

(**  Program parser *)
let pprogram = many1 (token bind <* choice [ token (str_token ";;"); empty ])

let parse str = parse_str pprogram (String.strip str)

(**  TESTS *)

let show_parsed_result str parser show =
  match parse_str parser str with
  | Ok res -> Format.printf "%s" (show res)
  | Error e -> Format.printf "Error%s" e
;;

(**  Consts tests *)

let%expect_test _ =
  show_parsed_result "777" pconst show_const;
  [%expect {| (CInt 777) |}]
;;

let%expect_test _ =
  show_parsed_result "-777" pconst show_const;
  [%expect {| (CInt -777) |}]
;;

let%expect_test _ =
  show_parsed_result "true" pconst show_const;
  [%expect {| (CBool true) |}]
;;

let%expect_test _ =
  show_parsed_result "false" pconst show_const;
  [%expect {| (CBool false) |}]
;;

let%expect_test _ =
  show_parsed_result "()" pconst show_const;
  [%expect {| CUnit |}]
;;

(**  Pattern tests *)

let%expect_test _ =
  show_parsed_result "777" pattern show_pattern;
  [%expect {|
    (PConst (CInt 777)) |}]
;;

let%expect_test _ =
  show_parsed_result "a" pattern show_pattern;
  [%expect {|
    (PVar "a") |}]
;;

let%expect_test _ =
  show_parsed_result "()" pattern show_pattern;
  [%expect {|
    (PConst CUnit) |}]
;;

let%expect_test _ =
  show_parsed_result "_" pattern show_pattern;
  [%expect {|
    PWild |}]
;;

(**  Expression tests *)

(**  Binary operations *)

let%expect_test _ =
  show_parsed_result "x + 1" expr show_expr;
  [%expect {|
    (EBinOp (Add, (EVar "x"), (EConst (CInt 1)))) |}]
;;

let%expect_test _ =
  show_parsed_result "x - 1" expr show_expr;
  [%expect {|
    (EBinOp (Sub, (EVar "x"), (EConst (CInt 1)))) |}]
;;

let%expect_test _ =
  show_parsed_result "x + y - 1" expr show_expr;
  [%expect
    {|
    (EBinOp (Sub, (EBinOp (Add, (EVar "x"), (EVar "y"))), (EConst (CInt 1)))) |}]
;;

let%expect_test _ =
  show_parsed_result "x * y / z" expr show_expr;
  [%expect {|
    (EBinOp (Div, (EBinOp (Mul, (EVar "x"), (EVar "y"))), (EVar "z"))) |}]
;;

let%expect_test _ =
  show_parsed_result "x * (y / z)" expr show_expr;
  [%expect {|
    (EBinOp (Mul, (EVar "x"), (EBinOp (Div, (EVar "y"), (EVar "z"))))) |}]
;;

let%expect_test _ =
  show_parsed_result "(x + y) * (z - y) / (y * x)  *  m" expr show_expr;
  [%expect
    {|
    (EBinOp (Mul,
       (EBinOp (Div,
          (EBinOp (Mul, (EBinOp (Add, (EVar "x"), (EVar "y"))),
             (EBinOp (Sub, (EVar "z"), (EVar "y"))))),
          (EBinOp (Mul, (EVar "y"), (EVar "x"))))),
       (EVar "m"))) |}]
;;

let%expect_test _ =
  show_parsed_result "x % 100" expr show_expr;
  [%expect {|
    (EBinOp (Mod, (EVar "x"), (EConst (CInt 100)))) |}]
;;

let%expect_test _ =
  show_parsed_result "x < 1" expr show_expr;
  [%expect {|
    (EBinOp (Less, (EVar "x"), (EConst (CInt 1)))) |}]
;;

let%expect_test _ =
  show_parsed_result "x > 1" expr show_expr;
  [%expect {|
    (EBinOp (Gre, (EVar "x"), (EConst (CInt 1)))) |}]
;;

let%expect_test _ =
  show_parsed_result "x <= 1" expr show_expr;
  [%expect {|
    (EBinOp (Leq, (EVar "x"), (EConst (CInt 1)))) |}]
;;

let%expect_test _ =
  show_parsed_result "x >= 1" expr show_expr;
  [%expect {|
    (EBinOp (Geq, (EVar "x"), (EConst (CInt 1)))) |}]
;;

let%expect_test _ =
  show_parsed_result "x = y" expr show_expr;
  [%expect {|
    (EBinOp (Eq, (EVar "x"), (EVar "y"))) |}]
;;

let%expect_test _ =
  show_parsed_result "x <> 1" expr show_expr;
  [%expect {|
    (EBinOp (Neq, (EVar "x"), (EConst (CInt 1)))) |}]
;;

let%expect_test _ =
  show_parsed_result "true || x > 1 || y > 1 || false" expr show_expr;
  [%expect
    {|
    (EBinOp (Or,
       (EBinOp (Or,
          (EBinOp (Or, (EConst (CBool true)),
             (EBinOp (Gre, (EVar "x"), (EConst (CInt 1)))))),
          (EBinOp (Gre, (EVar "y"), (EConst (CInt 1)))))),
       (EConst (CBool false)))) |}]
;;

let%expect_test _ =
  show_parsed_result "x && y || z && w" expr show_expr;
  [%expect
    {|
    (EBinOp (Or, (EBinOp (And, (EVar "x"), (EVar "y"))),
       (EBinOp (And, (EVar "z"), (EVar "w"))))) |}]
;;

let%expect_test _ =
  show_parsed_result "x || y && z || w" expr show_expr;
  [%expect
    {|
    (EBinOp (Or,
       (EBinOp (Or, (EVar "x"), (EBinOp (And, (EVar "y"), (EVar "z"))))),
       (EVar "w"))) |}]
;;

(**  Conditions *)

let%expect_test _ =
  show_parsed_result "if true then 1 else 2" expr show_expr;
  [%expect {|
    (EIf ((EConst (CBool true)), (EConst (CInt 1)), (EConst (CInt 2)))) |}]
;;

let%expect_test _ =
  show_parsed_result "if x % 5 = 0 then x * 5 else (x - x % 5) * 5" expr show_expr;
  [%expect
    {|
    (EIf (
       (EBinOp (Eq, (EBinOp (Mod, (EVar "x"), (EConst (CInt 5)))),
          (EConst (CInt 0)))),
       (EBinOp (Mul, (EVar "x"), (EConst (CInt 5)))),
       (EBinOp (Mul,
          (EBinOp (Sub, (EVar "x"), (EBinOp (Mod, (EVar "x"), (EConst (CInt 5))))
             )),
          (EConst (CInt 5))))
       )) |}]
;;

(**  Lambda functions *)

let%expect_test _ =
  show_parsed_result "fun x -> x" expr show_expr;
  [%expect {|
    (EFun ((PVar "x"), (EVar "x"))) |}]
;;

let%expect_test _ =
  show_parsed_result
    "fun x y z-> fun t -> if t > 0 then (x + y + z - t) / t else 0"
    expr
    show_expr;
  [%expect
    {|
    (EFun ((PVar "x"),
       (EFun ((PVar "y"),
          (EFun ((PVar "z"),
             (EFun ((PVar "t"),
                (EIf ((EBinOp (Gre, (EVar "t"), (EConst (CInt 0)))),
                   (EBinOp (Div,
                      (EBinOp (Sub,
                         (EBinOp (Add, (EBinOp (Add, (EVar "x"), (EVar "y"))),
                            (EVar "z"))),
                         (EVar "t"))),
                      (EVar "t"))),
                   (EConst (CInt 0))))
                ))
             ))
          ))
       )) |}]
;;

(**  Applications *)

let%expect_test _ =
  show_parsed_result "f x" expr show_expr;
  [%expect {|
    (EApp ((EVar "f"), (EVar "x"))) |}]
;;

let%expect_test _ =
  show_parsed_result "(fun x y z -> x * 100) x y z" expr show_expr;
  [%expect
    {|
    (EApp (
       (EApp (
          (EApp (
             (EFun ((PVar "x"),
                (EFun ((PVar "y"),
                   (EFun ((PVar "z"),
                      (EBinOp (Mul, (EVar "x"), (EConst (CInt 100))))))
                   ))
                )),
             (EVar "x"))),
          (EVar "y"))),
       (EVar "z"))) |}]
;;

(**  Let and let rec *)

let%expect_test _ =
  show_parsed_result "let f x = x" pprogram show_program;
  [%expect {|
    [(ELet (false, "f", (EFun ((PVar "x"), (EVar "x")))))] |}]
;;

let%expect_test _ =
  show_parsed_result "let bind x f = f x" pprogram show_program;
  [%expect
    {|
    [(ELet (false, "bind",
        (EFun ((PVar "x"), (EFun ((PVar "f"), (EApp ((EVar "f"), (EVar "x")))))))
        ))
      ] |}]
;;

let%expect_test _ =
  show_parsed_result "let mult x = fun y -> x * y" pprogram show_program;
  [%expect
    {|
    [(ELet (false, "mult",
        (EFun ((PVar "x"),
           (EFun ((PVar "y"), (EBinOp (Mul, (EVar "x"), (EVar "y")))))))
        ))
      ] |}]
;;

let%expect_test _ =
  show_parsed_result "let x = 10" pprogram show_program;
  [%expect {| [(ELet (false, "x", (EConst (CInt 10))))] |}]
;;

let%expect_test _ =
  show_parsed_result
    "let rec factorial n =\n  if n <= 1\n  then 1\n  else factorial (n - 1) * n"
    pprogram
    show_program;
  [%expect
    {|
    [(ELet (true, "factorial",
        (EFun ((PVar "n"),
           (EIf ((EBinOp (Leq, (EVar "n"), (EConst (CInt 1)))),
              (EConst (CInt 1)),
              (EBinOp (Mul,
                 (EApp ((EVar "factorial"),
                    (EBinOp (Sub, (EVar "n"), (EConst (CInt 1)))))),
                 (EVar "n")))
              ))
           ))
        ))
      ] |}]
;;

let%expect_test _ =
  show_parsed_result
    "let rec series n = if n = 1 then 1 else n + series (n - 1)"
    pprogram
    show_program;
  [%expect
    {|
    [(ELet (true, "series",
        (EFun ((PVar "n"),
           (EIf ((EBinOp (Eq, (EVar "n"), (EConst (CInt 1)))), (EConst (CInt 1)),
              (EBinOp (Add, (EVar "n"),
                 (EApp ((EVar "series"),
                    (EBinOp (Sub, (EVar "n"), (EConst (CInt 1))))))
                 ))
              ))
           ))
        ))
      ] |}]
;;

let%expect_test _ =
  show_parsed_result
    "let rec fibonacci n = if n <= 1 then 1 else fibonacci (n - 1) + fibonacci (n - 2)"
    pprogram
    show_program;
  [%expect
    {|
    [(ELet (true, "fibonacci",
        (EFun ((PVar "n"),
           (EIf ((EBinOp (Leq, (EVar "n"), (EConst (CInt 1)))),
              (EConst (CInt 1)),
              (EBinOp (Add,
                 (EApp ((EVar "fibonacci"),
                    (EBinOp (Sub, (EVar "n"), (EConst (CInt 1)))))),
                 (EApp ((EVar "fibonacci"),
                    (EBinOp (Sub, (EVar "n"), (EConst (CInt 2))))))
                 ))
              ))
           ))
        ))
      ] |}]
;;

let%expect_test _ =
  show_parsed_result
    "let fack1 k n m = k (n * m);; let rec fack n k = if n <= 1 then k 1 else fack (n-1) \
     (fack1 k n);; let id x = x;; let fac n = fack n id"
    pprogram
    show_program;
  [%expect
    {|
    [(ELet (false, "fack1",
        (EFun ((PVar "k"),
           (EFun ((PVar "n"),
              (EFun ((PVar "m"),
                 (EApp ((EVar "k"), (EBinOp (Mul, (EVar "n"), (EVar "m")))))))
              ))
           ))
        ));
      (ELet (true, "fack",
         (EFun ((PVar "n"),
            (EFun ((PVar "k"),
               (EIf ((EBinOp (Leq, (EVar "n"), (EConst (CInt 1)))),
                  (EApp ((EVar "k"), (EConst (CInt 1)))),
                  (EApp (
                     (EApp ((EVar "fack"),
                        (EBinOp (Sub, (EVar "n"), (EConst (CInt 1)))))),
                     (EApp ((EApp ((EVar "fack1"), (EVar "k"))), (EVar "n")))))
                  ))
               ))
            ))
         ));
      (ELet (false, "id", (EFun ((PVar "x"), (EVar "x")))));
      (ELet (false, "fac",
         (EFun ((PVar "n"),
            (EApp ((EApp ((EVar "fack"), (EVar "n"))), (EVar "id")))))
         ))
      ] |}]
;;

let%expect_test _ =
  show_parsed_result
    "let id x = x;;\n\
    \     let acc1 acc x y = acc (x + y);;\n\
    \     let acc2 fib_func n acc x = fib_func (n - 2) (acc1 acc x);;\n\
    \     let rec fibo_cps n acc = if n < 3 then acc 1 else fibo_cps (n - 1) (acc2 \
     fibo_cps n acc);;\n\
    \     let fibo n = fibo_cps n id"
    pprogram
    show_program;
  [%expect
    {|
    [(ELet (false, "id", (EFun ((PVar "x"), (EVar "x")))));
      (ELet (false, "acc1",
         (EFun ((PVar "acc"),
            (EFun ((PVar "x"),
               (EFun ((PVar "y"),
                  (EApp ((EVar "acc"), (EBinOp (Add, (EVar "x"), (EVar "y")))))))
               ))
            ))
         ));
      (ELet (false, "acc2",
         (EFun ((PVar "fib_func"),
            (EFun ((PVar "n"),
               (EFun ((PVar "acc"),
                  (EFun ((PVar "x"),
                     (EApp (
                        (EApp ((EVar "fib_func"),
                           (EBinOp (Sub, (EVar "n"), (EConst (CInt 2)))))),
                        (EApp ((EApp ((EVar "acc1"), (EVar "acc"))), (EVar "x")))
                        ))
                     ))
                  ))
               ))
            ))
         ));
      (ELet (true, "fibo_cps",
         (EFun ((PVar "n"),
            (EFun ((PVar "acc"),
               (EIf ((EBinOp (Less, (EVar "n"), (EConst (CInt 3)))),
                  (EApp ((EVar "acc"), (EConst (CInt 1)))),
                  (EApp (
                     (EApp ((EVar "fibo_cps"),
                        (EBinOp (Sub, (EVar "n"), (EConst (CInt 1)))))),
                     (EApp (
                        (EApp ((EApp ((EVar "acc2"), (EVar "fibo_cps"))),
                           (EVar "n"))),
                        (EVar "acc")))
                     ))
                  ))
               ))
            ))
         ));
      (ELet (false, "fibo",
         (EFun ((PVar "n"),
            (EApp ((EApp ((EVar "fibo_cps"), (EVar "n"))), (EVar "id")))))
         ))
      ] |}]
;;

(**  Let in and let rec in *)

let%expect_test _ =
  show_parsed_result "let sum x = fun y -> x + y in sum 10 5" expr show_expr;
  [%expect
    {|
    (ELetIn (false, "sum",
       (EFun ((PVar "x"),
          (EFun ((PVar "y"), (EBinOp (Add, (EVar "x"), (EVar "y"))))))),
       (EApp ((EApp ((EVar "sum"), (EConst (CInt 10)))), (EConst (CInt 5)))))) |}]
;;

let%expect_test _ =
  show_parsed_result
    "let rec fact x = if x <= 1 then 1 else fact (x - 1) * x in fact 10"
    expr
    show_expr;
  [%expect
    {|
    (ELetIn (true, "fact",
       (EFun ((PVar "x"),
          (EIf ((EBinOp (Leq, (EVar "x"), (EConst (CInt 1)))), (EConst (CInt 1)),
             (EBinOp (Mul,
                (EApp ((EVar "fact"),
                   (EBinOp (Sub, (EVar "x"), (EConst (CInt 1)))))),
                (EVar "x")))
             ))
          )),
       (EApp ((EVar "fact"), (EConst (CInt 10)))))) |}]
;;
