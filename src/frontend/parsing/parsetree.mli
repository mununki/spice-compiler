open Ast.Asttypes

type varname = string [@@deriving show]

type expression =
  | Pconstant of loc * constant
  | PVar of loc * varname
  (* variable *)
  | PApp of loc * expression * expression
  (* application: e1 e2 *)
  | PLam of loc * varname * expression
  (* abstraction: fun x -> e *)
  | PLet of loc * varname * expression * expression
  (* let x = e in e2 *)
  | PBinOp of loc * bin_op * expression * expression

and constant =
  | Pconst_integer of int
  | Pconst_boolean of bool
  | Pconst_string of string

type program = Prog of expression list
