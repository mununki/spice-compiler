{
  open Lexing
  open Parser

  exception SyntaxError of string
}

let whitespace = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"

let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']

let int = '-'? digit+
let id = (alpha) (alpha|digit|'_')*

rule read_token = parse
| "->" { ARROW }
| "(" { LPAREN }
| ")" { RPAREN }
| "=" { EQUAL }
| "+" { PLUS }
| "-" { MINUS }
| "*" { MULT }
| "/" { DIV }
| "fun" { LAMBDA }
| "let" { LET }
| "in" { IN }
| "true" { TRUE }
| "false" { FALSE }
| whitespace { read_token lexbuf }
| int { INT (int_of_string (Lexing.lexeme lexbuf)) }
| id { ID (Lexing.lexeme lexbuf) }
| '"' { read_string (Buffer.create 17) lexbuf }
| newline { new_line lexbuf; read_token lexbuf }
| eof { EOF }
| _ { raise (SyntaxError ("Lexer - Illegal character: " ^ Lexing.lexeme lexbuf)) }

and read_string buf = parse
| '"' { STRING (Buffer.contents buf) }
| '\\' '/' { Buffer.add_char buf '/'; read_string buf lexbuf }
| '\\' '\\' { Buffer.add_char buf '\\'; read_string buf lexbuf }
| '\\' 'b' { Buffer.add_char buf '\b'; read_string buf lexbuf }
| '\\' 'f' { Buffer.add_char buf '\012'; read_string buf lexbuf }
| '\\' 'n' { Buffer.add_char buf '\n'; read_string buf lexbuf }
| '\\' 'r' { Buffer.add_char buf '\r'; read_string buf lexbuf }
| '\\' 't' { Buffer.add_char buf '\t'; read_string buf lexbuf }
| [^ '"' '\\']+
  { Buffer.add_string buf (Lexing.lexeme lexbuf); read_string buf lexbuf }
| _ { raise (SyntaxError ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }
| eof { raise (SyntaxError ("String is not terminated"))}
