open Ast.Asttypes

type varname = string [@@deriving show]

type expression =
  | Pconstant of loc * constant
  | PVar of loc * varname
  | PApp of loc * expression * expression
  | PLam of loc * varname * expression
  | PLet of loc * varname * expression * expression
  | PBinOp of loc * bin_op * expression * expression

and constant =
  | Pconst_integer of int
  | Pconst_boolean of bool
  | Pconst_string of string

type program = Prog of expression list
