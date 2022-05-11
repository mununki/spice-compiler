type loc = Lexing.position
type bin_op = BinOpPlus | BinOpMinus | BinOpMult | BinOpDiv

val string_of_loc : loc -> string
val string_of_bin_op : bin_op -> string
