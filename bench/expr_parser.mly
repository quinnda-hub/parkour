%{
open Expr_ast
%}

%token <int> INT
%token PLUS MINUS TIMES DIV LPAREN RPAREN EOF

%left PLUS MINUS
%left TIMES DIV
%nonassoc UMINUS

%start <Expr_ast.t> main

%%

main: e = expr EOF { e }

expr:
  | n = INT { Int n }
  | LPAREN e = expr RPAREN { e }
  | MINUS e = expr %prec UMINUS { Neg e }
  | a = expr PLUS b = expr { Bin ('+', a, b) }
  | a = expr MINUS b = expr { Bin ('-', a, b) }
  | a = expr TIMES b = expr { Bin ('*', a, b) }
  | a = expr DIV b = expr { Bin ('/', a, b) }
