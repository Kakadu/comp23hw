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

  let get_parse_error env =
    match I.stack env with
    | (lazy Nil) -> "Invalid syntax"
    | (lazy (Cons (I.Element (state, _, _, _), _))) -> (
        try Parser_messages.message (I.number state)
        with Not_found ->
          "invalid syntax (no specific message for this error)")

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
