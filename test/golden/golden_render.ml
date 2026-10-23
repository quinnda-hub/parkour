open Parkour

let header title = Printf.printf "=== %s ===\n" title

(* Make the escape byte visible in the expected file. *)
let visible s = String.concat "\\e" (String.split_on_char '\027' s)

let show ?location ?color ?max_context ?tab_width ?max_width src d =
  print_string
    (visible
       (Diagnostic.render ?location ?color ?max_context ?tab_width ?max_width
          src d))

let first_error p text =
  match parse p text with
  | Error (d :: _) -> d
  | _ -> failwith "expected a parse error"

let case ?name ?location ?color ?max_context ?tab_width ?max_width title p text
    =
  header title;
  show ?location ?color ?max_context ?tab_width ?max_width
    (Source.make ?name text) (first_error p text)

(* The span of the first occurrence of [sub] in [text]. *)
let find text sub =
  let n = String.length sub in
  let rec go i = if String.sub text i n = sub then i else go (i + 1) in
  let start = go 0 in
  Span.make ~start ~stop:(start + n)

let at text ?(len = 0) sub =
  let s = find text sub in
  Span.make ~start:s.start ~stop:(s.start + len)

let diag_case ?name ?location ?color ?max_context ?tab_width ?max_width title
    text d =
  header title;
  show ?location ?color ?max_context ?tab_width ?max_width
    (Source.make ?name text) d

let is_alpha = function 'a' .. 'z' -> true | _ -> false
let is_digit = function '0' .. '9' -> true | _ -> false
let ident = take_while1 "identifier" is_alpha

let assignment =
  context "assignment"
    (let+ _ =
       ident
       <* skip_while (( = ) ' ')
       <* char '=' <* cut
       <* skip_while (( = ) ' ')
     and+ v = take_while1 "integer" is_digit in
     v)

let () =
  case ~name:"config.txt" "single line, expectations (label shown)" assignment
    "ammo = banana";
  case "two expectations, no file name"
    (ident *> (char ';' <|> char ','))
    "abc!";
  case ~name:"limits.cfg" "custom error with code (no label)"
    (validate ~code:"E42"
       (fun s ->
         if int_of_string s > 255 then Error "value out of range" else Ok s)
       (take_while1 "integer" is_digit))
    "300";
  diag_case ~name:"a.txt" "warning" "x = 1 # old"
    (Diagnostic.make ~severity:`Warning ~code:"W1"
       ~span:(at "x = 1 # old" ~len:5 "# old")
       "deprecated comment style");
  case "unexpected only (not_followed_by)"
    (not_followed_by (string "let"))
    "let x";
  case "end of input after a trailing newline: empty last line"
    (char 'a' *> char '\n' *> char 'b')
    "a\n";
  case "end of input without trailing newline" (char 'a' *> char 'b') "a";
  case "empty input" (char 'a') "";
  let block = "first line\nsecond line\nthird line\nfourth" in
  let multi_line =
    validate
      (fun _ -> Error "block is not allowed here")
      (take_while1 "text" (fun c -> c <> '.'))
  in
  case "multi-line span, 4 lines (no elision)" multi_line block;
  let long_block =
    String.concat "\n"
      (List.init 11 (fun i -> Printf.sprintf "line %d" (i + 1)))
  in
  case "multi-line span, more than 4 lines (elided, 2-digit gutter)" multi_line
    long_block;
  let tabs = "\tkey\t=\toops" in
  diag_case "tabs expanded (tab_width 4)" tabs
    (Diagnostic.make ~expected:[ Label "integer" ] ~span:(at tabs ~len:4 "oops")
       "expected integer");
  diag_case ~tab_width:8 "tabs expanded (tab_width 8)" tabs
    (Diagnostic.make ~span:(at tabs ~len:4 "oops") "bad value");
  let wide = "\xe5\x90\x8d\xe5\x89\x8d = e\xcc\x81 \xe5\x80\xa4!" in
  diag_case "wide (CJK) chars and a combining character before the span" wide
    (Diagnostic.make ~span:(at wide ~len:4 "\xe5\x80\xa4!") "bad value");
  let crlf = "a = 1\r\nb = x\r\nc = 3\r\n" in
  case "CRLF input" ~name:"crlf.txt"
    (many
       (ident
       *> skip_while (( = ) ' ')
       *> char '='
       *> skip_while (( = ) ' ')
       *> take_while1 "integer" is_digit
       *> char '\r' *> char '\n'))
    crlf;
  let odd = "ok \xff\xfe \x01 \x7f \r end" in
  diag_case "invalid UTF-8 and control characters in the line" odd
    (Diagnostic.make
       ~span:(at odd ~len:6 "\xff\xfe \x01 \x7f")
       "unexpected bytes");
  let long = String.make 150 'a' ^ "BAD" ^ String.make 150 'b' in
  diag_case "very long line: window cut on both sides" long
    (Diagnostic.make ~span:(at long ~len:3 "BAD") "bad word");
  diag_case ~max_width:40 "long line, max_width 40, cut on the left only" long
    (Diagnostic.make ~span:(Span.point (String.length long)) "unexpected end");
  diag_case ~max_width:40 "long line, max_width 40, cut on the right only" long
    (Diagnostic.make ~span:(Span.point 0) "bad start");
  let mid = "let x = (1 +\n  2 *\n  3) end" in
  diag_case "multi-line span from mid-line 1 to mid-line 3, with expectations"
    mid
    (Diagnostic.make
       ~expected:[ Label "expression"; Literal ";" ]
       ~span:
         (Span.make ~start:(find mid "(1").start
            ~stop:((find mid ") end").start + 1))
       "bad expression");
  let odd2 = "a\xc2\x85b\xe2\x80\xaeZ" in
  diag_case "C1 control and U+202E shown as U+FFFD" odd2
    (Diagnostic.make ~span:(at odd2 ~len:3 "\xe2\x80\xaeZ") "bad text");
  let frames n =
    List.init n (fun i ->
        (Printf.sprintf "level %d" (i + 1), Span.make ~start:0 ~stop:i))
  in
  let ctx_text = "abcdefgh" in
  diag_case "context trimming: 5 frames, default max_context" ctx_text
    (Diagnostic.make ~context:(frames 5)
       ~span:(Span.make ~start:6 ~stop:7)
       "bad");
  diag_case ~max_context:4 "context: 5 frames, max_context 4 (nothing omitted)"
    ctx_text
    (Diagnostic.make ~context:(frames 5)
       ~span:(Span.make ~start:6 ~stop:7)
       "bad");
  let loc_text = "caf\xc3\xa9 = \xc3\xa9\xc3\xa9 oops" in
  let loc_d = Diagnostic.make ~span:(at loc_text ~len:4 "oops") "bad value" in
  diag_case ~name:"loc.txt" ~location:`Char_offset
    "location style: char offset, with name" loc_text loc_d;
  diag_case ~location:`Byte_offset "location style: byte offset, no name"
    loc_text loc_d;
  diag_case ~color:true "colour (escape bytes shown as \\e)" "ammo = banana\n"
    (Diagnostic.make ~code:"E1"
       ~expected:[ Label "integer"; Literal "-" ]
       ~context:[ ("assignment", Span.make ~start:0 ~stop:7) ]
       ~span:(Span.make ~start:7 ~stop:13)
       "expected integer or '-', found \"banana\"");
  let text = "caf\xc3\xa9 = 1\nxx = oops\n" in
  let d =
    Diagnostic.make ~code:"E7" ~span:(at text ~len:4 "oops") "bad value"
  in
  header "compact pp: with source (named)";
  Format.printf "%a@."
    (Diagnostic.pp_located (Source.make ~name:"f.txt" text))
    d;
  header "compact pp: with source (unnamed, char offset)";
  Format.printf "%a@."
    (Diagnostic.pp_located ~location:`Char_offset (Source.make text))
    d;
  header "compact pp: without source";
  Format.printf "%a@." Diagnostic.pp d;
  header "compact pp: without source, empty span, warning";
  Format.printf "%a@." Diagnostic.pp
    (Diagnostic.make ~severity:`Warning ~span:(Span.point 5) "odd")

(* Text primitives: the library's own messages *)

module L =
  (val Lexeme.make ~line_comments:[ "#" ]
         ~block_comments:[ ("/*", "*/") ]
         ~reserved:[ "let"; "in" ] ())

let () =
  case ~name:"limits.txt" "integer out of range"
    (string "max = " *> integer)
    "max = 99999999999999999999\n";
  case "number out of range" (char '[' *> number) "[1.5e999, 2]";
  case "unterminated string (newline)" string_literal
    "\"never closed\nnext line";
  case "unterminated string (end of input)" string_literal "\"abc\\";
  case "invalid escape sequence" string_literal "\"tab\\q here\"";
  case "invalid unicode escape (lone surrogate)" string_literal
    "\"emoji \\ud83d here\"";
  case "invalid unicode escape (short)" string_literal "\"\\u12G4\"";
  case "unterminated comment"
    (L.symbol "x" *> L.symbol "=" *> L.lexeme integer)
    "x = 1 /* started here\nand never closed\n";
  case "invalid UTF-8" (many Utf8.any) "caf\xe9 au lait";
  case "identifier on a reserved word" L.identifier "let = 1";
  case "keyword on a longer identifier" (L.keyword "if") "iffy = 1"

(* Operator expressions *)

let () =
  let module L = (val Lexeme.make ()) in
  (* Lexeme operators: underlines must not cover the whitespace they skip. *)
  let op s f = L.symbol s >>| fun _ -> f in
  let term = L.lexeme (satisfy "letter" is_letter) >>| String.make 1 in
  let node name a b = Printf.sprintf "(%s %s %s)" name a b in
  let expr =
    L.space
    *> expression term
         [
           [ Prefix (op "-" (fun a -> "(- " ^ a ^ ")")) ];
           [ Postfix (op "!" (fun a -> "(" ^ a ^ " !)")) ];
           [ Infix_left (op "+" (node "+")) ];
           [ Infix_none (op "<" (node "<")) ];
         ]
  in
  case "operator cannot be chained" expr "a < b < c";
  case "expression: operand expected after an operator" expr "a + * b";
  case "expression: operator expected after an operand" expr "a ! b"
