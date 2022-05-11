open Core
open Parsing
open Typing

let usage_msg = "append [-verbose]  [] ... -o "

let verbose = ref false

let input_files = ref []

let output_file = ref ""

let anon_fun filename = input_files := filename :: !input_files

let speclist =
  [
    ("-verbose", Arg.Set verbose, "Output debug information");
    ("-o", Arg.Set_string output_file, "Set output file name");
  ]

let pprint_error = Fmt.pf Format.std_formatter "%s"

let () =
  Arg.parse speclist anon_fun usage_msg;
  match !input_files |> List.hd with
  | Some input_file -> (
      let inx = In_channel.create input_file in
      let lexbuf = Lexing.from_channel inx in
      let open Result in
      let typedtree =
        lexbuf |> Parse.parse_program >>= Type_program.type_program
      in
      match typedtree with
      | Ok tt -> Type_program.pprint_typed_ast Format.std_formatter tt
      | Error error -> pprint_error (Error.to_string_hum error))
  | None -> pprint_error "need file to compile"

(* TEST *)
(* let () =
   let lexbuf = "let x = fun y -> y + 1 in x" |> Lexing.from_string in
   let open Result in
   let typedtree = lexbuf |> Parse.parse_program >>= Type_program.type_program in
   match typedtree with
   | Ok tt -> Type_program.pprint_typed_ast Format.std_formatter tt
   | Error error -> pprint_error (Error.to_string_hum error) *)
