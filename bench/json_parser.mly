%{
open Json_ast
%}

%token NULL TRUE FALSE LBRACK RBRACK LBRACE RBRACE COMMA COLON EOF
%token <float> NUMBER
%token <string> STRING

%start <Json_ast.t> main

%%

main: v = value EOF { v }

value:
  | NULL { Null }
  | TRUE { Bool true }
  | FALSE { Bool false }
  | n = NUMBER { Number n }
  | s = STRING { String s }
  | LBRACK items = separated_list(COMMA, value) RBRACK { Array items }
  | LBRACE members = separated_list(COMMA, member) RBRACE { Object members }

member: k = STRING COLON v = value { (k, v) }
