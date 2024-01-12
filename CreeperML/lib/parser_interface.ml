(** Copyright 2023-2024, Arthur Alekseev and Starcev Matvey *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(* https://www.baturin.org/blog/declarative-parse-error-reporting-with-menhir/ *)

module ParserInterface = struct
  open Lexing
  module I = Parser.MenhirInterpreter
  open Monad.Result

  exception Syntax_error of ((int * int) option * string)

  let get_lexing_position lexbuf =
    let p = Lexing.lexeme_start_p lexbuf in
    let line_number = p.Lexing.pos_lnum in
    let column = p.Lexing.pos_cnum - p.Lexing.pos_bol + 1 in
    (line_number, column)

  let message = function
    | 0 ->
        {|Wow, you caught error that I don't know when it appears! (O.O)
          Pls, send your input to me so that I can add cooler discription!|}
    | 1 ->
        {|After LET token and before REC/name-that-binding token
             appered something that I can't recognize (>.<)|}
    | 3 ->
        {|After LET/REC token and before name-that-binding token
             appered something that I can't recognize (>.<)|}
    | 6 ->
        {|Oh, boy, you put some nonsence between brackets,
          when you named your value of function (Y.Y)|}
    | 9 -> {|Um, you missed comma when declaring tuple to be named (I.I)|}
    | 10 | 14 ->
        {|Um, eto, eh! After comma in tuple declaration should be another lvalue! (I.I)|}
    | 13 ->
        {|You may missed RIGHTPARENT in tuple declaration or may COMMA! (P.P)|}
    | 16 ->
        {|Ups, between name to be binded and EQUALLY should not be nothing! (\\.\\)|}
    | 17 | 19 ->
        {|Mmmm, after binding name and possible arguments should be EQUALLY! (^.^)|}
    | 20 ->
        {|Yo, you should declarate function or value after EQUALLY token! (K.K)|}
    | 24 ->
        {|So, you either write expression after LEFTPARENT,
          or nothing to declarate unit! (G.G)|}
    | 26 -> {|God says, after IF word must be expression expression! (;;.;;)|}
    | 27 ->
        {|And our God says, after FUN token should be name of argument! (Z.Z)|}
    | 28 ->
        {|Yo, you should declarate function or value after ARROW token! (K.K)|}
    | 29 ->
        {|It's 4 am now, I am too sleepe, you puted somethin that isn't ARROW token
          after names of args in your FUN function! (6.^)|}
    | 30 -> {|Fff, after ARROW token must be something line let_body! (U.^)|}
    | 32 ->
        {|Before IN you added something that are not part of let-body expression! ($.$)|}
    | 34 -> {|After IN token must be some expression, I think so! (///.///)|}
    | 40 ->
        {|Honestly, this is bullshit (my code, not your). 
          I don't have enough contex, you may placed something
          that are not expression before PREDICATE xor THEN token! (:dead:)|}
    | 41 -> {|Between THEN and your then-code appered something! (@.@)|}
    | 42 ->
        {|Between ELSE and your then-code appered something!
          Xor between PREDICATE and first to be applied expression
          appered something, yes! (*.*)|}
    | 43 -> {|<YOUR SYNTAX ERROR MESSAGE HERE>|}
    | 45 | 47 | 52 | 54 ->
        {|Between PREDICATE and second to be applied expression
          appered something, yes! (*.*)|}
    | 49 | 56 ->
        {|After atomic expression exist thing that should not exist! (%.%)|}
    | 57 -> {|ELSE token and else-code were squeezed each other thing! (!.!)|}
    | 59 ->
        {|You wanna use tuple or place expression between brackets?
          But you placed bad thing before COMMA or RIGHTPARENT at each case!
          Xor thing be placed after expression and before PREDICATE! (F.F)|}
    | 61 ->
        {|At expression tuple you plased something that are not
          expression after first COMMA! (M.M)|}
    | 64 ->
        {|At your tuple you placed somthing that should not be place
          after one of expressions! 
          Xor you placed unexpected one between expression and PREDICATE! (A.A)|}
    | 65 -> {|After COMMA in tuple should be expression! (R.R)|}
    | 73 ->
        {|Your program ends not good xor your last let-statement end not good! (E.e)|}
    | _ ->
        {| ___     ___        _____                               __  __ _
          |   |   |   |      / ____|                             |  \/  | |
          |___|___|___|     | |     _ __ ___  ___ _ __   ___ _ __| \  / | |
             _|   |_        | |    | '__/ _ \/ _ \ '_ \ / _ \ '__| |\/| | |
            |  ___  |       | |____| | |  __/  __/ |_) |  __/ |  | |  | | |____
            |_|   |_|        \_____|_|  \___|\___| .__/ \___|_|  |_|  |_|______|
                                                            |_||}

  let get_parse_error env =
    match I.stack env with
    | (lazy Nil) -> "Invalid syntax"
    | (lazy (Cons (I.Element (state, _, _, _), _))) -> message (I.number state)

  let rec parse lexbuf checkpoint =
    match checkpoint with
    | I.InputNeeded _env ->
        let token = Lexer.token lexbuf in
        let startp = lexbuf.lex_start_p and endp = lexbuf.lex_curr_p in
        let checkpoint = I.offer checkpoint (token, startp, endp) in
        parse lexbuf checkpoint
    | I.Shifting _ | I.AboutToReduce _ ->
        let checkpoint = I.resume checkpoint in
        parse lexbuf checkpoint
    | I.HandlingError _env ->
        let line, pos = get_lexing_position lexbuf in
        let err = get_parse_error _env in
        raise (Syntax_error (Some (line, pos), err))
    | I.Accepted v -> v
    | I.Rejected ->
        raise
          (Syntax_error (None, "invalid syntax (parser rejected the input)"))

  let parser lexbuf =
    try
      let code = parse lexbuf (Parser.Incremental.parse lexbuf.lex_curr_p) in
      return code
    with Syntax_error (pos, err) -> (
      match pos with
      | Some (line, pos) ->
          Printf.sprintf "Syntax error on line %d, character %d:\n%s" line pos
            err
          |> error
      | None -> Printf.sprintf "Syntax error:\n%s" err |> error)

  let from_string s =
    let lexbuf = Lexing.from_string s in
    parser lexbuf

  let from_channel ic =
    let lexbuf = Lexing.from_channel ic in
    parser lexbuf

  let from_file file =
    let ic = open_in file in
    let program = from_channel ic in
    let () = close_in ic in
    program
end
