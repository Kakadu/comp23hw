(** Copyright 2023-2024, Rustam Shangareev and Danil Yevdokimov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

open Angstrom
open Ast
open Base

let parse p s = parse_string ~consume:All p s

let is_empty = function
  | ' ' | '\t' | '\r' | '\n' -> true
  | _ -> false
;;

let not_is_empty c = is_empty c |> not
let empty = take_while is_empty
let empty_lr p = empty *> p <* empty

let is_digit = function
  | '0' .. '9' -> true
  | _ -> false
;;

let is_lower = function
  | 'a' .. 'z' -> true
  | _ -> false
;;

let is_upper = function
  | 'A' .. 'Z' -> true
  | _ -> false
;;

let is_wild = function
  | '_' -> true
  | _ -> false
;;

let is_valid_ident c = is_digit c || is_lower c || is_upper c || is_wild c

let is_keyword = function
  | "let"
  | "in"
  | "true"
  | "false"
  | "rec"
  | "if"
  | "else"
  | "then"
  | "with"
  | "fun"
  | "function"
  | "match" -> true
  | _ -> false
;;

let token s = empty *> string s

let keyword s =
  token s
  <* (peek_char
      >>| function
      | Some x when is_valid_ident x -> fail "Incorrect keyword"
      | _ -> return None)
;;

let between l r p = l *> p <* r
let brackets p = token "(" *> p <* token ")"
let cint n = CInt n
let cbool b = CBool b
let econst c = EConst c
let eunop o e = EUnOp (o, e)
let evar id = EVar id
let eif e1 e2 e3 = EIf (e1, e2, e3)
let elet binds e = ELet (binds, e)
let efun p e = EFun (p, e)
let eapp = return (fun e1 e2 -> EApp (e1, e2))
let efun args rhs = List.fold_right args ~f:efun ~init:rhs
let ebinop o e1 e2 = EBinOp (o, e1, e2)
let ccase p e = p, e
let bbind isrec p e = isrec, p, e
let ptwild _ = PtWild
let ptvar id = PtVar id
let ptconst c = PtConst c
let dlet isrec p e = DLet (isrec, p, e)

let procr op pl pr =
  let p =
    fix @@ fun p -> pl >>= fun l -> op >>= (fun op -> p <|> pr >>| op l) <|> return l
  in
  p
;;

let procl op pl pr =
  let rec go acc =
    (fun f x -> f acc x) <$> op <*> choice [ pl >>= go; pr ] <|> return acc
  in
  pl >>= go
;;

let choice_op ops =
  choice @@ List.map ~f:(fun (tok, cons) -> token tok *> (return @@ ebinop cons)) ops
;;

let apply_unary p =
  choice
    [ token "-" *> p >>| eunop Minus
    ; keyword "not" *> p >>| eunop Not
    ; token "+" *> p
    ; p
    ]
;;

let id valid_fst =
  let* fst = empty *> satisfy valid_fst in
  let take_func =
    match fst with
    | '_' -> many1
    | _ -> many
  in
  let* inner = take_func @@ satisfy is_valid_ident in
  let id = Base.String.of_char_list @@ (fst :: inner) in
  if is_keyword id then fail "Keyword" else return id
;;

let ident =
  id
  @@ function
  | 'a' .. 'z' | '_' -> true
  | _ -> false
;;

let uns = empty_lr @@ take_while1 is_digit

let cunsint =
  let* uns = uns in
  return @@ Base.Int.of_string uns >>| cint
;;

let cint =
  let* sign = option "" (token "+" <|> token "-") in
  let* uns = empty_lr @@ take_while1 is_digit in
  return @@ Base.Int.of_string (sign ^ uns) >>| cint
;;

let cbool =
  let ctrue = keyword "true" *> return (cbool true) in
  let cfalse = keyword "false" *> return (cbool false) in
  ctrue <|> cfalse
;;

let const = empty_lr @@ choice [ cint; cbool ]
let uns_const = empty_lr @@ choice [ cunsint; cbool ]
let ptvar = ident >>| ptvar
let ptwild = token "_" >>| ptwild
let ptconst = const >>| ptconst

type pdispatch =
  { other : pdispatch -> pt t
  ; pt : pdispatch -> pt t
  }

let chainl1 e op =
  let rec go acc = op >>| (fun f x -> f acc x) <*> e >>= go <|> return acc in
  e >>= go
;;

let pack =
  let pt d = fix @@ fun _self -> empty_lr @@ d.other d in
  let other d =
    fix
    @@ fun _self ->
    let prim = empty_lr @@ choice [ ptconst; ptvar; ptwild; brackets @@ d.pt d ] in
    prim
  in
  { other; pt }
;;

let pt = pack.pt pack

type edispatch =
  { key : edispatch -> exp t
  ; exp : edispatch -> exp t
  ; op : edispatch -> exp t
  }

let chainl1' i e op =
  let rec go acc = op >>| (fun f x -> f acc x) <*> e >>= go <|> return acc in
  i >>= go
;;

let pack =
  let pt = pt in
  let exp d = fix @@ fun _self -> empty_lr @@ d.key d <|> d.op d in
  let key d =
    fix
    @@ fun _self ->
    let eif =
      empty_lr
      @@ lift3
           eif
           (keyword "if" *> d.exp d)
           (keyword "then" *> d.exp d)
           (keyword "else" *> d.exp d)
    in
    let elet =
      let binding =
        empty_lr
          (bbind
           <$> keyword "let" *> option false (keyword "rec" >>| fun _ -> true)
           <*> pt
           <*> (efun <$> (empty *> many pt <* token "=") <*> (d.exp d <* keyword "in")))
      in
      elet <$> many1 binding <*> d.exp d
    in
    let efun = empty_lr (efun <$> (keyword "fun" *> many pt <* token "->") <*> d.exp d) in
    choice [ elet; eif; efun ]
  in
  let op d =
    fix
    @@ fun _self ->
    let prim =
      empty_lr @@ choice [ uns_const >>| econst; ident >>| evar; brackets @@ d.exp d ]
    in
    let app_op = empty_lr @@ chainl1' prim prim eapp in
    let mul_op = procl (choice_op [ "*", Mul; "/", Div ]) app_op @@ d.key d in
    let add_op =
      procl
        (choice_op [ "+", Add; "-", Sub ])
        (apply_unary mul_op)
        (apply_unary @@ d.key d)
    in
    let cons_op = fix @@ fun _ -> add_op >>= return in
    let cmp_op =
      procl (choice_op [ ">=", Geq; ">", Gre; "<=", Leq; "<", Less ]) cons_op @@ d.key d
    in
    let eq_op = procl (choice_op [ "=", Eq; "<>", Neq ]) cmp_op @@ d.key d in
    let conj_op = procl (choice_op [ "&&", And ]) eq_op @@ d.key d in
    let disj_op = procl (choice_op [ "||", Or ]) conj_op @@ d.key d in
    empty_lr disj_op
  in
  { key; op; exp }
;;

let decl =
  lift3
    dlet
    (keyword "let" *> option false (keyword "rec" >>| fun _ -> true))
    pt
    (efun <$> (empty *> many pt <* token "=") <*> pack.exp pack)
;;

let pprog (l : decl list) : prog = l
let prog = sep_by1 (token ";;") decl <* option "" @@ empty_lr (token ";;") >>| pprog
