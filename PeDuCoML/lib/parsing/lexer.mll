(** Copyright 2023-2024, Danila Pechenev, Ilya Dudnikov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

{
    open Menhirparser
}
let ws = [' ' '\n' '\t']+
let digit = ['0'-'9']
let nonzero_digit = ['1' - '9']
let char = ['a'-'z' 'A'-'Z' '0'-'9' '_' '-' ' ' ',' ';' ':' '(' ')' '[' ']' '\n' '?' '!']

let int = nonzero_digit digit* | digit
let string = '"' char* '"'
let char = ''' char '''
let bool = "true" | "false"

let first_symbol = ['a'-'z' 'A'-'Z']
let symbol = ['a'-'z' 'A'-'Z' '_' ''' '0'-'9']
let id = first_symbol symbol* | '_'+ ['a'-'z' 'A'-'Z' ''' '0'-'9'] symbol* 
let wildcard = '_'+

let binary_operation = ['+' '-' '*' '/' '=' '>' '<'] | "<>" | ">=" | "<=" | "&&" | "||"
let unary_operation = '-' | "not"

rule token = parse
    | ws  { token lexbuf }
    | int { INT (int_of_string (Lexing.lexeme lexbuf)) }
    | string { STRING (Lexing.lexeme lexbuf) }
    | char { CHAR ((Lexing.lexeme lexbuf).[1]) }
    | bool { BOOL (bool_of_string (Lexing.lexeme lexbuf)) }
    | "if" { IF }
    | "then" { THEN }
    | "else" { ELSE }
    | "fun" { FUN }
    | "let" { LET }
    | "in" { IN }
    | "and" { AND }
    | "match" { MATCH }
    | "with" { WITH }
    | '|' { OPTION }
    | "rec" { REC }
    | "->" { ARR }
    | ',' { COMMA }
    | ';' { SEMICOLON }
    | '(' { LBR }
    | ')' { RBR }
    | '[' { LSQBR }
    | ']' { RSQBR }
    | '=' { EQ }
    | "::" { CONS }
    (* unary operators *)
    (* binary operators *)
    | "||" { OR }
    | "&&" { LOG_AND }
    | "<>" { NEQ }
    | '>' { GT }
    | ">=" { GTE }
    | '<' { LT }
    | "<=" { LTE }
    | '+' { ADD }
    | '-' { SUB }
    | '*' { MUL }
    | '/' { DIV }
    | "not" { NOT }
    | id { IDENTIFIER (Lexing.lexeme lexbuf) }
    | wildcard { WILDCARD }
    | eof { EOF }
