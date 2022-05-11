open Ast.Asttypes
open Parsing.Parsetree
open Typecore

type expression =
  | Tconstr of loc * typ * constant
  | TVar of loc * typ * varname
  | TApp of loc * typ * expression * expression
  | TLam of loc * typ * varname * expression
  | TLet of loc * typ * varname * expression * expression
  | TBinOp of loc * typ * bin_op * expression * expression

type program = Prog of expression list

let env = { contents = [] }

let rec expression expr =
  let typ = typeof env expr in
  match expr with
  | Pconstant (loc, c) -> Tconstr (loc, typ, c)
  | PVar (loc, varname) -> TVar (loc, typ, varname)
  | PApp (loc, e1, e2) -> TApp (loc, typ, expression e1, expression e2)
  | PLam (loc, varname, e) -> TLam (loc, typ, varname, expression e)
  | PLet (loc, varname, e1, e2) ->
      TLet (loc, typ, varname, expression e1, expression e2)
  | PBinOp (loc, bin_op, e1, e2) ->
      TBinOp (loc, typ, bin_op, expression e1, expression e2)
