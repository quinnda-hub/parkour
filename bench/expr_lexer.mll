{
open Expr_parser

exception Error of string
}

rule token = parse
  | [' ' '\t' '\n' '\r']+ { token lexbuf }
  | ['0'-'9']+ { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | '+' { PLUS }
  | '-' { MINUS }
  | '*' { TIMES }
  | '/' { DIV }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | eof { EOF }
  | _ as c { raise (Error (Printf.sprintf "unexpected character %C" c)) }
