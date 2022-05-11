open Core
open Parsing

val type_program : Parsetree.program -> Typedtree.program Or_error.t
val pprint_typed_ast : Format.formatter -> Typedtree.program -> unit
