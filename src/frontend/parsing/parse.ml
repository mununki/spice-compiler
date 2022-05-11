open Base
open Lexer
open Lexing

let print_error_position lexbuf =
  let pos = lexbuf.lex_curr_p in
  Fmt.str "Line:%d Position:%d" pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let parse_program lexbuf =
  try Ok (Parser.program Lexer.read_token lexbuf) with
  | SyntaxError msg ->
      let error_msg = Fmt.str "%s: %s@." (print_error_position lexbuf) msg in
      Error (Error.of_string error_msg)
  | Parser.Error ->
      let error_msg =
        Fmt.str "%s: syntax error@" (print_error_position lexbuf)
      in
      Error (Error.of_string error_msg)

let pprint_parsed_ast ppf (prog : Parsetree.program) =
  Pprint_past.pprint_program ppf prog
