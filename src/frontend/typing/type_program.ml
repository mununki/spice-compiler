open Core
open Parsing

let type_program (Parsetree.Prog exprs) =
  try Ok (Typedtree.Prog (List.map ~f:Typedtree.expression exprs))
  with error -> Error (Error.of_exn error)

let pprint_typed_ast ppf (prog : Typedtree.program) =
  Pprint_tast.pprint_program ppf prog

let pprint_error = Fmt.pf Format.std_formatter "%s"

(* let%test _ =
   let lexbuf = "let x = y in x" |> Lexing.from_string in
   let open Result in
   let parsetree = lexbuf |> Parse.parse_program in
   (match parsetree with
   | Ok tt ->
       print_endline "success!";
       Pprint_past.pprint_program Format.std_formatter tt
   | Error error ->
       print_endline "error!";
       pprint_error (Error.to_string_hum error));
   true *)

let%test _ =
  let lexbuf = "let x = fun y -> y + 1 in x" |> Lexing.from_string in
  let open Result in
  let typedtree = lexbuf |> Parse.parse_program >>= type_program in
  (match typedtree with
  | Ok tt -> pprint_typed_ast Format.std_formatter tt
  | Error error -> pprint_error (Error.to_string_hum error));
  true
