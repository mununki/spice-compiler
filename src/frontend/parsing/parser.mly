%{
  open Ast.Asttypes
  open Parsetree
%}

%token ARROW
%token LPAREN
%token RPAREN
%token EQUAL
%token PLUS
%token MINUS
%token MULT
%token DIV
%token LAMBDA
%token LET
%token IN
%token TRUE
%token FALSE
%token <string> ID
%token <int> INT
%token <string> STRING
%token EOF

%left PLUS MINUS
%left MULT DIV

%start program

%type <Parsetree.program> program

%type <expression> expression

%type <bin_op> bin_op

%%

program:
| exprs=list(expression); EOF {Prog(exprs)}

expression:
| LPAREN; e=expression; RPAREN { e }
| TRUE { Pconstant($startpos, Pconst_boolean true) }
| FALSE { Pconstant($startpos, Pconst_boolean false) }
| i=INT { Pconstant($startpos, Pconst_integer i) }
| s=STRING { Pconstant($startpos, Pconst_string s) }
| variable=ID { PVar($startpos, variable) }
| e1=expression; op=bin_op; e2=expression { PBinOp($startpos, op, e1, e2) }
| LAMBDA; var_name=ID; ARROW; expr=expression { PLam($startpos, var_name, expr)}
| LPAREN; app_expr=expression; arg_expr=expression; RPAREN { PApp($startpos, app_expr, arg_expr) }
| LET; var_name=ID; EQUAL; bound_expr=expression; IN; expr=expression  { PLet($startpos, var_name, bound_expr, expr) }

%inline bin_op:
| PLUS { BinOpPlus }
| MINUS { BinOpMinus }
| MULT { BinOpMult }
| DIV { BinOpDiv }
