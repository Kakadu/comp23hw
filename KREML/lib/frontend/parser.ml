(** Copyright 2023-2024, Anton Kraev and Polina Badreeva *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Angstrom

(* helpers *)
let is_space = function
  | ' ' | '\t' | '\n' | '\r' -> true
  | _ -> false
;;

let skip = take_while is_space
let trim p = skip *> p <* skip
let parens p = skip *> char '(' *> skip *> p <* skip <* char ')'

let varname =
  let keywords =
    [ "fun"
    ; "val"
    ; "let"
    ; "in"
    ; "end"
    ; "if"
    ; "then"
    ; "else"
    ; "fn"
    ; "true"
    ; "false"
    ; "not"
    ; "orelse"
    ; "andalso"
    ]
  in
  let is_valid_first_char = function
    | 'a' .. 'z' | '_' -> true
    | _ -> false
  in
  let is_varname_char = function
    | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> true
    | _ -> false
  in
  skip *> peek_char
  >>= function
  | Some c when is_valid_first_char c ->
    take_while is_varname_char
    >>= fun s ->
    if List.exists (fun keyword -> keyword = s) keywords
    then fail "Invalid variable name: keyword used"
    else return s
  | _ -> fail "Invalid variable name: bad first char"
;;

(* dispatch *)
type dispatch =
  { literal_p : expr t
  ; identifier_p : expr t
  ; unary_op_p : dispatch -> expr t
  ; binary_op_p : dispatch -> expr t
  ; app_p : dispatch -> expr t
  ; abs_p : dispatch -> expr t
  ; if_then_else_p : dispatch -> expr t
  ; let_in_p : dispatch -> expr t
  }

(* binding parser *)
let binding_p d =
  fix
  @@ fun self ->
  let name keyword =
    string keyword *> skip *> d.identifier_p
    >>= function
    | EIdentifier id -> return id
    | _ -> fail "Invalid binding name"
  in
  let args = skip *> many (varname <* skip) in
  let body =
    skip
    *> string "="
    *> choice
         [ d.unary_op_p d
         ; d.binary_op_p d
         ; d.let_in_p d
         ; d.app_p d
         ; d.abs_p d
         ; d.if_then_else_p d
         ; d.literal_p
         ; d.identifier_p
         ]
  in
  skip
  *> (parens self <|> lift2 b_val (name "val") body <|> lift3 b_fun (name "fun") args body)
;;

(* expression parsers *)
let literal_p =
  fix
  @@ fun self ->
  let boolean = string "true" <|> string "false" >>| bool_of_string in
  let integer =
    take_while1 (function
      | '0' .. '9' -> true
      | _ -> false)
    >>| int_of_string
  in
  let int_literal = integer >>| lint in
  let bool_literal = boolean >>| lbool in
  let literal = choice [ int_literal; bool_literal ] in
  skip *> (parens self <|> lift e_literal literal)
;;

let identifier_p =
  fix
  @@ fun self ->
  let is_wildcard c = c == "_" in
  let id =
    varname
    >>= function
    | id when not (is_wildcard id) -> return @@ e_identifier id
    | _ -> fail "Invalid variable name"
  in
  skip *> (parens self <|> id)
;;

let unary_op_p d =
  fix
  @@ fun self ->
  let op = choice [ char '~' >>| uneg; string "not" >>| unot ] in
  let body =
    choice
      [ parens self
      ; d.binary_op_p d
      ; d.let_in_p d
      ; d.app_p d
      ; d.if_then_else_p d
      ; d.literal_p
      ; d.identifier_p
      ]
  in
  skip *> lift2 e_unary_op op body
;;

let binary_op_p d =
  fix
  @@ fun self ->
  let multiplicative = skip *> choice [ char '*' >>| bmul; char '/' >>| bdiv ] in
  let additive = skip *> choice [ char '+' >>| badd; char '-' >>| bsub ] in
  let relational =
    skip
    *> choice
         [ string ">=" >>| bgte
         ; string "<=" >>| blte
         ; char '>' >>| bgt
         ; char '<' >>| blt
         ]
  in
  let equality = skip *> string "=" >>| beq in
  let logical_and = skip *> string "andalso" >>| band in
  let logical_or = skip *> string "orelse" >>| bor in
  let rec parse_bin_op expr_parser op_parsers =
    let chainl1 expr_p op_p =
      let rec go acc =
        lift2 (fun f x -> e_binary_op f acc x) op_p expr_p >>= go <|> return acc
      in
      expr_p >>= fun init -> go init
    in
    match op_parsers with
    | [ op ] -> chainl1 expr_parser op
    | h :: t -> chainl1 (parse_bin_op expr_parser t) h
    | _ -> fail "Unreachable"
  in
  let body =
    choice
      [ parens self
      ; d.unary_op_p d
      ; d.let_in_p d
      ; d.app_p d
      ; d.if_then_else_p d
      ; d.literal_p
      ; d.identifier_p
      ]
  in
  skip
  *> parse_bin_op
       body
       [ logical_or; logical_and; equality; relational; additive; multiplicative ]
;;

let app_p d =
  fix
  @@ fun self ->
  let function_parser =
    choice
      [ parens @@ d.let_in_p d
      ; parens @@ d.abs_p d
      ; parens @@ d.if_then_else_p d
      ; identifier_p
      ]
  in
  let operand_parser =
    choice
      [ parens self
      ; parens @@ d.unary_op_p d
      ; parens @@ d.binary_op_p d
      ; parens @@ d.let_in_p d
      ; parens @@ d.abs_p d
      ; parens @@ d.if_then_else_p d
      ; literal_p
      ; identifier_p
      ]
  in
  let apply_lift acc = lift (e_app acc) operand_parser in
  let rec go acc = apply_lift acc >>= go <|> return acc in
  parens self <|> function_parser >>= fun init -> apply_lift init >>= fun init -> go init
;;

let abs_p d =
  fix
  @@ fun self ->
  parens self
  <|>
  let arg = skip *> string "fn" *> skip *> varname <* skip <* string "=>" in
  let body =
    choice
      [ self
      ; d.unary_op_p d
      ; d.binary_op_p d
      ; d.let_in_p d
      ; d.app_p d
      ; d.if_then_else_p d
      ; d.literal_p
      ; d.identifier_p
      ]
  in
  skip *> lift2 e_abs arg body
;;

let if_then_else_p d =
  fix
  @@ fun self ->
  parens self
  <|>
  let condition =
    choice
      [ self
      ; d.unary_op_p d
      ; d.binary_op_p d
      ; d.let_in_p d
      ; d.app_p d
      ; d.if_then_else_p d
      ; literal_p
      ; identifier_p
      ]
  in
  let branch =
    choice
      [ self
      ; d.unary_op_p d
      ; d.binary_op_p d
      ; d.let_in_p d
      ; d.app_p d
      ; d.abs_p d
      ; d.if_then_else_p d
      ; literal_p
      ; identifier_p
      ]
  in
  let cond = skip *> string "if" *> condition in
  let if_true = skip *> string "then" *> branch in
  let if_false = skip *> string "else" *> branch in
  skip *> lift3 e_if_then_else cond if_true if_false
;;

let let_in_p d =
  fix
  @@ fun self ->
  parens self
  <|>
  let expr =
    choice
      [ self
      ; d.unary_op_p d
      ; d.binary_op_p d
      ; d.app_p d
      ; d.abs_p d
      ; d.if_then_else_p d
      ; d.literal_p
      ; d.identifier_p
      ]
  in
  let bindings = string "let" *> skip *> sep_by1 skip (binding_p d) in
  let body = skip *> string "in" *> expr <* skip <* string "end" in
  skip *> lift2 e_let_in bindings body
;;

(* main parser *)
let dispatch =
  { literal_p
  ; identifier_p
  ; unary_op_p
  ; binary_op_p
  ; app_p
  ; abs_p
  ; if_then_else_p
  ; let_in_p
  }
;;

let parse program = parse_string ~consume:All (many (binding_p dispatch) <* skip) program
let parse_optimistically program = Result.get_ok (parse program)

let parse_error program =
  try
    let _ = parse_optimistically program in
    false
  with
  | Invalid_argument _ -> true
;;

(* tests *)

(*literal*)
let%test _ = parse_optimistically "val x = 55" = [ BVal ("x", ELiteral (LInt 55)) ]
let%test _ = parse_optimistically "val x = true" = [ BVal ("x", ELiteral (LBool true)) ]
(*identifier*)
let%test _ = parse_optimistically "val x = x" = [ BVal ("x", EIdentifier "x") ]

(*unary op*)

let%test _ =
  parse_optimistically "val x = ~55" = [ BVal ("x", EUnaryOp (Neg, ELiteral (LInt 55))) ]
;;

let%test _ =
  parse_optimistically "val x = not true"
  = [ BVal ("x", EUnaryOp (Not, ELiteral (LBool true))) ]
;;

let%test _ =
  parse_optimistically "val x = ~y" = [ BVal ("x", EUnaryOp (Neg, EIdentifier "y")) ]
;;

let%test _ =
  parse_optimistically "val x = not y" = [ BVal ("x", EUnaryOp (Not, EIdentifier "y")) ]
;;

(*binary op *)

let%test _ =
  parse_optimistically "val x = 5 + 3"
  = [ BVal ("x", EBinaryOp (Add, ELiteral (LInt 5), ELiteral (LInt 3))) ]
;;

let%test _ =
  parse_optimistically "val x = 8 / 2"
  = [ BVal ("x", EBinaryOp (Div, ELiteral (LInt 8), ELiteral (LInt 2))) ]
;;

let%test _ =
  parse_optimistically "val x = 5 <= 5"
  = [ BVal ("x", EBinaryOp (LtOrEq, ELiteral (LInt 5), ELiteral (LInt 5))) ]
;;

let%test _ =
  parse_optimistically "val x = 5 >= 3"
  = [ BVal ("x", EBinaryOp (GtOrEq, ELiteral (LInt 5), ELiteral (LInt 3))) ]
;;

let%test _ =
  parse_optimistically "val x = true andalso false"
  = [ BVal ("x", EBinaryOp (And, ELiteral (LBool true), ELiteral (LBool false))) ]
;;

let%test _ =
  parse_optimistically "val x = true orelse false"
  = [ BVal ("x", EBinaryOp (Or, ELiteral (LBool true), ELiteral (LBool false))) ]
;;

let%test _ =
  parse_optimistically "val x = (5 + 3) - 2"
  = [ BVal
        ( "x"
        , EBinaryOp
            (Sub, EBinaryOp (Add, ELiteral (LInt 5), ELiteral (LInt 3)), ELiteral (LInt 2))
        )
    ]
;;

let%test _ =
  parse_optimistically "val x = (8 / 2) * 3"
  = [ BVal
        ( "x"
        , EBinaryOp
            ( Mult
            , EBinaryOp (Div, ELiteral (LInt 8), ELiteral (LInt 2))
            , ELiteral (LInt 3) ) )
    ]
;;

let%test _ =
  parse_optimistically "val x = ~((5 <= 5) andalso (3 < 4))"
  = [ BVal
        ( "x"
        , EUnaryOp
            ( Neg
            , EBinaryOp
                ( And
                , EBinaryOp (LtOrEq, ELiteral (LInt 5), ELiteral (LInt 5))
                , EBinaryOp (Lt, ELiteral (LInt 3), ELiteral (LInt 4)) ) ) )
    ]
;;

let%test _ =
  parse_optimistically "val a = x + y"
  = [ BVal ("a", EBinaryOp (Add, EIdentifier "x", EIdentifier "y")) ]
;;

let%test _ =
  parse_optimistically "val a = x / y"
  = [ BVal ("a", EBinaryOp (Div, EIdentifier "x", EIdentifier "y")) ]
;;

let%test _ =
  parse_optimistically "val x = ((5 + (3 - 1)) * 2) / 3"
  = [ BVal
        ( "x"
        , EBinaryOp
            ( Div
            , EBinaryOp
                ( Mult
                , EBinaryOp
                    ( Add
                    , ELiteral (LInt 5)
                    , EBinaryOp (Sub, ELiteral (LInt 3), ELiteral (LInt 1)) )
                , ELiteral (LInt 2) )
            , ELiteral (LInt 3) ) )
    ]
;;

(*app*)

let%test _ =
  parse_optimistically "val x = f t"
  = [ BVal ("x", EApp (EIdentifier "f", EIdentifier "t")) ]
;;

let%test _ =
  parse_optimistically "val x = f (g t)"
  = [ BVal ("x", EApp (EIdentifier "f", EApp (EIdentifier "g", EIdentifier "t"))) ]
;;

let%test _ =
  parse_optimistically "val x = f (g (h t))"
  = [ BVal
        ( "x"
        , EApp
            ( EIdentifier "f"
            , EApp (EIdentifier "g", EApp (EIdentifier "h", EIdentifier "t")) ) )
    ]
;;

let%test _ =
  parse_optimistically "fun appp y x = y x"
  = [ BFun ("appp", [ "y"; "x" ], EApp (EIdentifier "y", EIdentifier "x")) ]
;;

(*abs*)

let%test _ =
  parse_optimistically "val y = fn x => x + 1"
  = [ BVal ("y", EAbs ("x", EBinaryOp (Add, EIdentifier "x", ELiteral (LInt 1)))) ]
;;

let%test _ =
  parse_optimistically "val y = fn x => fn z => (x + z)"
  = [ BVal ("y", EAbs ("x", EAbs ("z", EBinaryOp (Add, EIdentifier "x", EIdentifier "z"))))
    ]
;;

let%test _ =
  parse_optimistically "val y = fn x => fn z => fn w => (x + (z + w))"
  = [ BVal
        ( "y"
        , EAbs
            ( "x"
            , EAbs
                ( "z"
                , EAbs
                    ( "w"
                    , EBinaryOp
                        ( Add
                        , EIdentifier "x"
                        , EBinaryOp (Add, EIdentifier "z", EIdentifier "w") ) ) ) ) )
    ]
;;

(*if then else*)

let%test _ =
  parse_optimistically "val x = if true then 1 else 0"
  = [ BVal ("x", EIfThenElse (ELiteral (LBool true), ELiteral (LInt 1), ELiteral (LInt 0)))
    ]
;;

let%test _ =
  parse_optimistically "fun ifthen y x z = if y then x else z"
  = [ BFun
        ( "ifthen"
        , [ "y"; "x"; "z" ]
        , EIfThenElse (EIdentifier "y", EIdentifier "x", EIdentifier "z") )
    ]
;;

let%test _ =
  parse_optimistically "val x = if y >= 4 then (fn z => (z+1)) else f"
  = [ BVal
        ( "x"
        , EIfThenElse
            ( EBinaryOp (GtOrEq, EIdentifier "y", ELiteral (LInt 4))
            , EAbs ("z", EBinaryOp (Add, EIdentifier "z", ELiteral (LInt 1)))
            , EIdentifier "f" ) )
    ]
;;

let%test _ =
  parse_optimistically "fun ifthen y x z = if (y andalso not x) then z else ~z"
  = [ BFun
        ( "ifthen"
        , [ "y"; "x"; "z" ]
        , EIfThenElse
            ( EBinaryOp (And, EIdentifier "y", EUnaryOp (Not, EIdentifier "x"))
            , EIdentifier "z"
            , EUnaryOp (Neg, EIdentifier "z") ) )
    ]
;;

let%test _ =
  parse_optimistically "val x = if (f t) then 1 else 0"
  = [ BVal
        ( "x"
        , EIfThenElse
            (EApp (EIdentifier "f", EIdentifier "t"), ELiteral (LInt 1), ELiteral (LInt 0))
        )
    ]
;;

let%test _ =
  parse_optimistically "val x = if y > 4 then if z > 10 then 1 else 0 else 0"
  = [ BVal
        ( "x"
        , EIfThenElse
            ( EBinaryOp (Gt, EIdentifier "y", ELiteral (LInt 4))
            , EIfThenElse
                ( EBinaryOp (Gt, EIdentifier "z", ELiteral (LInt 10))
                , ELiteral (LInt 1)
                , ELiteral (LInt 0) )
            , ELiteral (LInt 0) ) )
    ]
;;

(*let in *)

let%test _ =
  parse_optimistically "val y = let val x = 5 in x * 2 end"
  = [ BVal
        ( "y"
        , ELetIn
            ( [ BVal ("x", ELiteral (LInt 5)) ]
            , EBinaryOp (Mult, EIdentifier "x", ELiteral (LInt 2)) ) )
    ]
;;

let%test _ = parse_optimistically "fun f x = x" = [ BFun ("f", [ "x" ], EIdentifier "x") ]

let%test _ =
  parse_optimistically "val y = let fun f x = x * 2 in (f 5) end"
  = [ BVal
        ( "y"
        , ELetIn
            ( [ BFun ("f", [ "x" ], EBinaryOp (Mult, EIdentifier "x", ELiteral (LInt 2))) ]
            , EApp (EIdentifier "f", ELiteral (LInt 5)) ) )
    ]
;;

let%test _ =
  parse_optimistically "val z = let fun f x = x val y = 1 in ((f 1 )* y) end"
  = [ BVal
        ( "z"
        , ELetIn
            ( [ BFun ("f", [ "x" ], EIdentifier "x"); BVal ("y", ELiteral (LInt 1)) ]
            , EBinaryOp (Mult, EApp (EIdentifier "f", ELiteral (LInt 1)), EIdentifier "y")
            ) )
    ]
;;

let%test _ =
  parse_optimistically "val y = let val x = 5 in x end"
  = [ BVal ("y", ELetIn ([ BVal ("x", ELiteral (LInt 5)) ], EIdentifier "x")) ]
;;

let%test _ =
  parse_optimistically "val y = let fun f x = x in f 5 end"
  = [ BVal
        ( "y"
        , ELetIn
            ( [ BFun ("f", [ "x" ], EIdentifier "x") ]
            , EApp (EIdentifier "f", ELiteral (LInt 5)) ) )
    ]
;;

let%test _ =
  parse_optimistically "fun factorial n = if n <= 1 then 1 else n * factorial (n - 1)"
  = [ BFun
        ( "factorial"
        , [ "n" ]
        , EIfThenElse
            ( EBinaryOp (LtOrEq, EIdentifier "n", ELiteral (LInt 1))
            , ELiteral (LInt 1)
            , EBinaryOp
                ( Mult
                , EIdentifier "n"
                , EApp
                    ( EIdentifier "factorial"
                    , EBinaryOp (Sub, EIdentifier "n", ELiteral (LInt 1)) ) ) ) )
    ]
;;

let%test _ =
  parse_optimistically "val result = factorial 3"
  = [ BVal ("result", EApp (EIdentifier "factorial", ELiteral (LInt 3))) ]
;;

let%test _ =
  parse_optimistically
    "fun factorial n = if n <= 1 then 1 else n * factorial (n - 1) val result = \
     factorial 3"
  = [ BFun
        ( "factorial"
        , [ "n" ]
        , EIfThenElse
            ( EBinaryOp (LtOrEq, EIdentifier "n", ELiteral (LInt 1))
            , ELiteral (LInt 1)
            , EBinaryOp
                ( Mult
                , EIdentifier "n"
                , EApp
                    ( EIdentifier "factorial"
                    , EBinaryOp (Sub, EIdentifier "n", ELiteral (LInt 1)) ) ) ) )
    ; BVal ("result", EApp (EIdentifier "factorial", ELiteral (LInt 3)))
    ]
;;

let%test _ =
  parse_optimistically "val x = 8 / 2 * 3 + f x"
  = [ BVal
        ( "x"
        , EBinaryOp
            ( Add
            , EBinaryOp
                ( Mult
                , EBinaryOp (Div, ELiteral (LInt 8), ELiteral (LInt 2))
                , ELiteral (LInt 3) )
            , EApp (EIdentifier "f", EIdentifier "x") ) )
    ]
;;

(*tests that should fail*)
let%test _ = parse_error "5"
let%test _ = parse_error "val x = True"
let%test _ = parse_error "val x = ~(fn x => (x + 1))"
let%test _ = parse_error "val y = (fn x => (x + 1)) + (fn x => (x + 1))"
let%test _ = parse_error "val x = (4+5) t"
let%test _ = parse_error "val x = if (fn x => x + 1) then (fn z => (z+1)) else f"
let%test _ = parse_error "val Ll = True"
