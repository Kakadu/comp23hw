(** Copyright 2023-2024, Arthur Alekseev and Starcev Matvey *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

%{
    open Parser_ast
    open ParserAstUtils
%}

%token <int> INT
%token <float> FLOAT
%token <string> STRING
%token <bool> BOOL
%token <string>NAME 
%token UNDERBAR
%token LET REC IN
%token FUN ARROW
%token IF THEN ELSE
%token LEFTPARENT RIGHTPARENT
%token COMMA
%token EQUALLY

%token <string>HIGHLVLPREDICATE
%token <string>MIDHIGHLVLPREDICATE
%token <string>MIDLVLPREDICATE
%token <string>LOWMIDLVLPREDICATE
%token <string>LOWLVLPREDICATE

%left LOWLVLPREDICATE
%left LOWMIDLVLPREDICATE
%left MIDLVLPREDICATE
%left MIDHIGHLVLPREDICATE
%left HIGHLVLPREDICATE

%token EOF

%type <Parser_ast.ParserAst.program> parse

%start parse

%%


parse : program EOF { $1 }

program : nonempty_list(let_binding) { $1 }

let_binding : LET rec_f lvalue list(lvalue) EQUALLY let_body 
    { let_binding ~rec_flag:$2 $startpos $endpos $3 $4 (build_let_body $startpos $endpos $4 $6) }

rec_f : 
    | REC { rec_f }
    | { norec_f }

let_body : list(inner_let_bind) expr { let_body $startpos $endpos $1 $2 }

inner_let_bind : let_binding IN { $1 }

unit : LEFTPARENT RIGHTPARENT { }

lvalue : 
    | UNDERBAR { lv_any $startpos $endpos }
    | unit { lv_unit $startpos $endpos }
    | NAME { lv_value $startpos $endpos $1 }
    | LEFTPARENT lvalue COMMA separated_nonempty_list(COMMA, lvalue) RIGHTPARENT { $2 :: $4 |> lv_tuple $startpos $endpos }

literal : 
    | INT { l_int $startpos $endpos $1 }
    | FLOAT { l_float $startpos $endpos $1 }
    | BOOL { l_bool $startpos $endpos $1 }
    | STRING { l_string $startpos $endpos $1 }
    | unit { l_unit $startpos $endpos }

expr :
    | atom { $1 }
    | FUN lvalue list(lvalue) ARROW let_body { build_mul_e_fun $startpos $endpos $2 $3 $5 }
    | apply expr { e_apply $startpos $endpos $1 $2 }
    | IF expr THEN expr ELSE expr { e_if_else $startpos $endpos $2 $4 $6 }
    | expr predicate expr { e_apply $startpos $endpos $2 $1 |> fun e -> e_apply $startpos $endpos e $3}

atom :
    | LEFTPARENT expr RIGHTPARENT { $2 }
    | literal { e_literal $startpos $endpos $1 }
    | NAME { e_value $startpos $endpos $1 }
    | LEFTPARENT expr COMMA separated_nonempty_list(COMMA, expr) RIGHTPARENT { $2 :: $4 |> e_tuple $startpos $endpos }

apply :
    | apply atom { e_apply $startpos $endpos $1 $2 }
    | atom { $1 }

%inline predicate :
    | HIGHLVLPREDICATE { e_value $startpos $endpos $1 }
    | MIDHIGHLVLPREDICATE { e_value $startpos $endpos $1 }
    | MIDLVLPREDICATE { e_value $startpos $endpos $1 }
    | LOWMIDLVLPREDICATE { e_value $startpos $endpos $1 }
    | LOWLVLPREDICATE { e_value $startpos $endpos $1 }