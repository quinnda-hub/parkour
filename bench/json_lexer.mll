{
open Json_parser

exception Error of string

let add_code buf code =
  if Uchar.is_valid code then Buffer.add_utf_8_uchar buf (Uchar.of_int code)
  else raise (Error "invalid \\u escape")
}

let ws = [' ' '\t' '\n' '\r']
let digit = ['0'-'9']
let hex = ['0'-'9' 'a'-'f' 'A'-'F']
let int = '-'? ('0' | ['1'-'9'] digit*)
let frac = '.' digit+
let exp = ['e' 'E'] ['+' '-']? digit+

rule token = parse
  | ws+ { token lexbuf }
  | "null" { NULL }
  | "true" { TRUE }
  | "false" { FALSE }
  | int frac? exp? { NUMBER (float_of_string (Lexing.lexeme lexbuf)) }
  | '"' { string (Buffer.create 16) lexbuf }
  | '[' { LBRACK }
  | ']' { RBRACK }
  | '{' { LBRACE }
  | '}' { RBRACE }
  | ',' { COMMA }
  | ':' { COLON }
  | eof { EOF }
  | _ as c { raise (Error (Printf.sprintf "unexpected character %C" c)) }

and string buf = parse
  | '"' { STRING (Buffer.contents buf) }
  | [^ '"' '\\' '\000'-'\031']+ as s { Buffer.add_string buf s; string buf lexbuf }
  | "\\\"" { Buffer.add_char buf '"'; string buf lexbuf }
  | "\\\\" { Buffer.add_char buf '\\'; string buf lexbuf }
  | "\\/" { Buffer.add_char buf '/'; string buf lexbuf }
  | "\\b" { Buffer.add_char buf '\b'; string buf lexbuf }
  | "\\f" { Buffer.add_char buf '\012'; string buf lexbuf }
  | "\\n" { Buffer.add_char buf '\n'; string buf lexbuf }
  | "\\r" { Buffer.add_char buf '\r'; string buf lexbuf }
  | "\\t" { Buffer.add_char buf '\t'; string buf lexbuf }
  | "\\u" (hex hex hex hex as h)
    { let hi = int_of_string ("0x" ^ h) in
      if hi >= 0xD800 && hi < 0xDC00 then
        (* a high surrogate needs its low half *)
        let lo = low lexbuf in
        add_code buf (0x10000 + ((hi - 0xD800) lsl 10) + (lo - 0xDC00))
      else add_code buf hi;
      string buf lexbuf }
  | eof { raise (Error "unterminated string") }
  | _ { raise (Error "invalid string") }

and low = parse
  | "\\u" (['d' 'D'] ['c'-'f' 'C'-'F'] hex hex as h) { int_of_string ("0x" ^ h) }
  | _ | eof { raise (Error "unpaired surrogate") }
