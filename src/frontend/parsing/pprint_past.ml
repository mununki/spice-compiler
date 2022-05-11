open Base
open Ast.Asttypes
open Parsetree

let indent_space = "   "

let rec pprint_expr ppf ~indent expr =
  let print_expr = Fmt.pf ppf "%sExpr: %s@." indent in
  let new_indent = indent_space ^ indent in
  match expr with
  | Pconstant (_, constant) -> (
      match constant with
      | Pconst_integer i -> print_expr (Fmt.str "Int:%d" i)
      | Pconst_boolean b -> print_expr (Fmt.str "Bool:%b" b)
      | Pconst_string s -> print_expr (Fmt.str "String:%s" s))
  | PVar (_, var_name) -> print_expr (Fmt.str "Variable: %s" var_name)
  | PApp (_, app_expr, arg_expr) ->
      pprint_expr ppf ~indent:new_indent app_expr;
      pprint_expr ppf ~indent:new_indent arg_expr
  | PLam (_, var_name, expr) ->
      print_expr (Fmt.str "Lam: %s" var_name);
      pprint_expr ppf ~indent:new_indent expr
  | PLet (_, var_name, bound_expr, expr) ->
      print_expr (Fmt.str "Let %s" var_name);
      pprint_expr ppf ~indent:new_indent bound_expr;
      pprint_expr ppf ~indent:new_indent expr
  | PBinOp (_, bin_op, expr1, expr2) ->
      print_expr (Fmt.str "Bin Op: %s" (string_of_bin_op bin_op));
      pprint_expr ppf ~indent:new_indent expr1;
      pprint_expr ppf ~indent:new_indent expr2

let pprint_program ppf (Prog exprs) =
  Fmt.pf ppf "Program@.";
  let indent = "└──" in
  List.iter ~f:(pprint_expr ppf ~indent) exprs
