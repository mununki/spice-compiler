open Ast.Asttypes
open Parsing
open Typecore

type expression =
  | Tconstr of loc * typ * Parsetree.constant
  | TVar of loc * typ * Parsetree.varname
  | TApp of loc * typ * expression * expression
  | TLam of loc * typ * Parsetree.varname * expression
  | TLet of loc * typ * Parsetree.varname * expression * expression
  | TBinOp of loc * typ * bin_op * expression * expression

type program = Prog of expression list

val expression : Parsetree.expression -> expression
