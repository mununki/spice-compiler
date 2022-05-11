open Core

val parse_program : Lexing.lexbuf -> Parsetree.program Or_error.t
val pprint_parsed_ast : Format.formatter -> Parsetree.program -> unit
