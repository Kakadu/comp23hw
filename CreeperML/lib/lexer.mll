(** Copyright 2023-2024, Arthur Alekseev and Starcev Matvey *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

{
    open Parser
}

let digit = ['0'-'9']
let sign = ['-' '+']

let int_const = sign? digit+
let float_const = sign? digit* '.' digit+
let bool_cosnt = "true" | "false"
let str_const = '"' ['a'-'z' 'A'-'Z' '0'-'9' '_' ' ' '.' ',' ':' ';' '(' ')']* '"'

let startname = ['a'-'z' 'A'-'Z' ''']
let tailname = ['a'-'z' 'A'-'Z' ''' '_' '0'-'9']
let name = startname tailname*

let bin_op = ['.' '>' '<' '=' '-' '/' '|' '!' '*' '+' '-' ':' '%' '@']
let h_prio_op = '.' bin_op*
let mh_prio_op = '*' '*' bin_op*
let m_prio_op = ['*' '/' '%'] bin_op*
let lm_prio_op = ['+' '-'] bin_op*
let l_prio_op = bin_op*

let whitespace = [' ' '\t' '\n' '\r']+

rule token = parse
    | whitespace { token lexbuf }
    | int_const { INT (Lexing.lexeme lexbuf |> int_of_string) }
    | float_const { FLOAT (Lexing.lexeme lexbuf |> float_of_string)}
    | bool_cosnt { BOOL (Lexing.lexeme lexbuf |> bool_of_string)}
    | str_const { STRING (Lexing.lexeme lexbuf) }
    | '_' { UNDERBAR }
    | "rec" { REC }
    | "in" { IN }
    | "let" { LET }
    | "fun" { FUN }
    | "if" { IF }
    | "then" { THEN }
    | "else" { ELSE }
    | "->" { ARROW }
    | ',' { COMMA }
    | '=' { EQUALLY }
    | ')' { RIGHTPARENT }
    | '(' { LEFTPARENT }
    | h_prio_op { HIGHLVLPREDICATE (Lexing.lexeme lexbuf) }
    | mh_prio_op { MIDHIGHLVLPREDICATE (Lexing.lexeme lexbuf) }
    | m_prio_op { MIDLVLPREDICATE (Lexing.lexeme lexbuf) }
    | lm_prio_op { LOWMIDLVLPREDICATE (Lexing.lexeme lexbuf) }
    | l_prio_op { LOWLVLPREDICATE (Lexing.lexeme lexbuf) }
    | name { NAME (Lexing.lexeme lexbuf) }
    | eof { EOF }
    | _ { raise (Failure ("Character not allowed in source text: '" ^ Lexing.lexeme lexbuf ^ "'")) }