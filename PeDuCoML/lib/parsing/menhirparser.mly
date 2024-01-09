(** Copyright 2023-2024, Danila Pechenev, Ilya Dudnikov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

%{
    open Ast
%}

// literals
%token <int> INT
%token <string> STRING
%token <char> CHAR
%token <bool> BOOL

// simple constructs
%token IF
%token THEN
%token ELSE
%token FUN
%token LET
%token IN
%token REC
%token AND
%token MATCH
%token WITH

%token WILDCARD

// separators
%token ARR
%token COMMA
%token SEMICOLON
%token OPTION

// braces
%token LBR
%token RBR
%token LSQBR
%token RSQBR

%token CONS

// unary operators
%token NOT

// binary operators
%token OR
%left OR
%token LOG_AND
%left LOG_AND
%token EQ
%left EQ
%token NEQ
%left NEQ
%token GT
%left GT
%token GTE
%left GTE
%token LT
%left LT
%token LTE
%left LTE
%token ADD
%left ADD
%token SUB
%left SUB
%token DIV
%left DIV
%token MUL
%left MUL
%token <string> IDENTIFIER
%token EOF

%nonassoc low
%nonassoc high

%type <declaration list> parse

%start parse

%%

parse : p = program EOF { p }
program : p = list(declaration) { p }
declaration : 
    | LET REC id = identifier p_list = list(pattern) EQ e = expr { drecursivedeclaration id p_list e }
    | LET id = identifier p_list = list(pattern) EQ e = expr { ddeclaration id p_list e }

pattern : 
    | l = literal { pliteral l }
    | t = ptuple { t }
    | p = plist { p }
    | p = pcons { p }
    | p = pwildcard { p }
    | id = identifier { 
        if String.for_all (fun c -> c = '_') id
        then pwildcard ()
        else pidentifier id 
    }
    | LBR p = pattern RBR { p }

pwildcard : 
    | WILDCARD { pwildcard () }
    | LBR p = pwildcard RBR { p }

ptuple :
    | t = ptuple_simple {
        let fst, tail = Base.List.hd_exn t, Base.List.tl_exn t in
        let snd, tail = Base.List.hd_exn tail, Base.List.tl_exn tail in
        ptuple fst snd tail
    }
    | LBR t = ptuple RBR { t }

ptuple_simple :
    | e1 = ptuple_elems COMMA e2 = ptuple_elems { [e1; e2] }
    | e = ptuple_elems COMMA t = ptuple_simple { e :: t }

%inline ptuple_elems : 
    | l = literal { pliteral l }
    | LBR t = ptuple RBR { t }
    | p = pwildcard { p }
    | p = plist { p }
    | p = pcons { p }
    | id = identifier { 
        if String.for_all (fun c -> c = '_') id
        then pwildcard ()
        else pidentifier id 
    } 

plist : 
    | LSQBR lst = separated_list(SEMICOLON, pattern) RSQBR { plist lst }
    | LBR l = plist RBR { l }

pcons : 
    | head = pcons_content CONS tail = pcons_content { pconstruct_list head tail }
    | LBR p = pcons RBR { p }

%inline pcons_content :
    | l = literal { pliteral l }
    | LBR t = ptuple RBR { t }
    | p = plist { p }
    | p = pwildcard { p }
    | p = pcons { p }
    | id = identifier { 
        if String.for_all (fun c -> c = '_') id
        then pwildcard ()
        else pidentifier id 
    }



expr : 
    | e = binary_operation { e }
    | e = unary_operation { e }
    | e = lam { e }
    | e = lst { e }
    | e = list_cons { e }
    | e = tuple { e }
    | e = ite { e }
    | l = letin { l }
    | pm = pattern_matching { pm }
    | app = application { app }
    | l = literal { eliteral l }
    | id = identifier { eidentifier id }
    | LBR e = expr RBR { e }


tuple :
    | t = tuple_simple {
        let fst, tail = Base.List.hd_exn t, Base.List.tl_exn t in
        let snd, tail = Base.List.hd_exn tail, Base.List.tl_exn tail in
        etuple fst snd tail
    }
    | LBR t = tuple RBR { t }

tuple_simple :
    | e1 = tuple_elems COMMA e2 = tuple_elems { [e1; e2] }
    | e = tuple_elems COMMA t = tuple_simple { e :: t }

%inline tuple_elems : 
    | LBR e = tuple RBR { e } 
    | l = letin { l }
    | e = list_cons { e }
    | e = binary_operation { e }
    | e = unary_operation { e }
    | e = lst { e }
    | app = application { app }
    | e = lam { e }
    | e = ite { e }
    | pm = pattern_matching { pm }
    | l = literal { eliteral l }
    | id = identifier { eidentifier id }
    
binary_operation :
    | l = expr op = binop r = expr { ebinary_operation op l r }

unary_operation : op = unop arg = unop_content { eunary_operation op arg }

%inline unop_content : 
    | LBR e = unary_operation RBR { e }
    | LBR e = letin RBR { e }
    | LBR e = application RBR { e }
    | LBR e = ite RBR { e }
    | LBR e = pattern_matching RBR { e }
    | l = literal { eliteral l }
    | id = identifier { eidentifier id } 
    | LBR e = binary_operation RBR { e }

application : func = l_app arg = r_app { eapplication func arg }

l_app :
    | LBR func = letin RBR { func }
    | LBR func = lam RBR { func }
    | LBR func = ite RBR { func }
    | LBR func = pattern_matching RBR { func }
    | func = application { func }
    | id = identifier { eidentifier id }

r_app :
    | LBR arg = tuple RBR { arg }
    | LBR arg = list_cons RBR { arg }
    | LBR arg = binary_operation RBR { arg }
    | LBR arg = unary_operation RBR { arg }
    | arg = lst { arg }
    | LBR arg = application RBR { arg }
    | LBR arg = letin RBR { arg }
    | LBR arg = lam RBR { arg }
    | LBR arg = ite RBR { arg }
    | LBR arg = pattern_matching RBR { arg }
    | l = literal { eliteral l }
    | id = identifier { eidentifier id }

lam : 
    | FUN fst = pattern pat_list = list(pattern) ARR body = expr { efun fst pat_list body }
    | LBR l = lam RBR { l }

lst : 
    | LSQBR lst = separated_list(SEMICOLON, expr) RSQBR { elist lst }
    | LBR l = lst RBR { l }

list_cons : head = expr CONS tail = expr { econstruct_list head tail }

ite : 
    | IF cond = expr THEN true_branch = expr ELSE false_branch = expr { eif cond true_branch false_branch }
    | LBR i = ite RBR { i }

letin : 
    | d = declaration IN body = expr { eletin d [] body }
    | LET REC id = identifier p_list = list(pattern) EQ e = expr
      AND decl_list = separated_nonempty_list(AND, letin_EQing) IN body = expr { 
        eletin (drecursivedeclaration id p_list e) (Base.List.map decl_list ~f:(fun (id, p_list, body) -> drecursivedeclaration id p_list body)) body
      } 
    | LET id = identifier p_list = list(pattern) EQ e = expr  
      AND decl_list = separated_nonempty_list(AND, letin_EQing) IN body = expr { 
        eletin (ddeclaration id p_list e) (Base.List.map decl_list ~f:(fun (id, p_list, body) -> ddeclaration id p_list body)) body
      }
    | LBR l = letin RBR { l }
letin_EQing:
    | id = identifier pat_list = list(pattern) EQ body = expr { (id, pat_list, body) }

pattern_matching : 
    | MATCH scrutinee = expr WITH p = pattern ARR action = expr { ematchwith scrutinee (p, action) [] }
    | MATCH scrutinee = expr WITH OPTION p = pattern ARR action = expr { ematchwith scrutinee (p, action) [] }
    | MATCH scrutinee = expr WITH fst_pat = pattern ARR fst_act = expr OPTION rest = separated_nonempty_list(OPTION, matching_case) { ematchwith scrutinee (fst_pat, fst_act) rest }
    | MATCH scrutinee = expr WITH OPTION fst_pat = pattern ARR fst_act = expr OPTION rest = separated_nonempty_list(OPTION, matching_case) { ematchwith scrutinee (fst_pat, fst_act) rest }
    | LBR pm = pattern_matching RBR { pm }

matching_case :
    | case = pattern ARR action = expr { (case, action) }

literal : 
    | i = INT { lint i }
    | s = STRING { lstring_with_quotes s }
    | c = CHAR { lchar c }
    | b = BOOL { lbool b }
    | LBR l = literal RBR { l }

identifier : 
    | id = IDENTIFIER { id } 
    | LBR id = identifier RBR { id }
%inline binop : 
    | OR { OR }
    | LOG_AND { AND }
    | EQ { Eq }
    | NEQ { NEq }
    | GT { GT }
    | GTE { GTE }
    | LT { LT }
    | LTE { LTE }
    | ADD { Add }
    | SUB { Sub }
    | MUL { Mul }
    | DIV { Div }
%inline unop :
    | SUB { Minus }
    | NOT { Not }

