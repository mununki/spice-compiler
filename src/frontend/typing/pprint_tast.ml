open Ast.Asttypes
open Typedtree
open Typecore

let indent_space = "   "

let rec pprint_expr ppf ~indent expr =
  let print_expr = Fmt.pf ppf "%sExpr: %s@." indent in
  let rec typ_to_string = function
    | TVar { contents = Unbound (tname, _) } -> "TVar: %s" ^ tname
    | TVar { contents = Link typ } -> typ_to_string typ
    | TArrow (t1, t2, _) ->
        "TArrow: " ^ typ_to_string t1 ^ " -> " ^ typ_to_string t2
    | Tconstr_integer _ -> "Tconstr_integer"
    | Tconstr_boolean _ -> "Tconstr_boolean"
    | Tconstr_string _ -> "Tconstr_string"
  in
  let new_indent = indent_space ^ indent in
  match expr with
  | Tconstr (_, _, c) -> (
      match c with
      | Pconst_boolean b -> print_expr (Fmt.str "Bool:%b" b)
      | Pconst_integer i -> print_expr (Fmt.str "Int:%d" i)
      | Pconst_string s -> print_expr (Fmt.str "String:%s" s))
  | TVar (_, typ, var_name) ->
      print_expr (Fmt.str "Var: %s: %s" var_name (typ_to_string typ))
  | TApp (_, typ, app_expr, arg_expr) ->
      print_expr (Fmt.str "App: %s" (typ_to_string typ));
      pprint_expr ppf ~indent:new_indent app_expr;
      pprint_expr ppf ~indent:new_indent arg_expr
  | TLam (_, typ, varname, expr) ->
      print_expr (Fmt.str "Lambda: %s: %s" varname (typ_to_string typ));
      pprint_expr ppf ~indent:new_indent expr
  | TLet (_, typ, var_name, bound_expr, expr) ->
      print_expr (Fmt.str "Let var: %s: %s" var_name (typ_to_string typ));
      pprint_expr ppf ~indent:new_indent bound_expr;
      pprint_expr ppf ~indent:new_indent expr
  | TBinOp (_, typ, bin_op, expr1, expr2) ->
      print_expr
        (Fmt.str "Bin Op: %s: %s" (string_of_bin_op bin_op) (typ_to_string typ));
      pprint_expr ppf ~indent:new_indent expr1;
      pprint_expr ppf ~indent:new_indent expr2

let pprint_program ppf (Prog exprs) =
  Fmt.pf ppf "Program@.";
  let indent = "└──" in
  List.iter (pprint_expr ppf ~indent) exprs
