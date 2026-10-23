(* Mini again (see mini.ml), this time in two steps: a lexer turns the text
   into tokens, then a grammar over the tokens parses them. The grammar is
   the one of mini.ml, minus everything about spaces, comments and
   spelling: it works on [Kw "let"] and [Sym ";"], not on characters.

   Run the built-in program: dune exec examples/mini_tokens.exe
   Parse a file:             dune exec examples/mini_tokens.exe -- program.mini
   Print the error gallery:  dune exec examples/mini_tokens.exe -- --gallery

   Diagnostics still point into the original text: every token carries the
   span of its source, so the parser's errors underline the right characters.

   Differences from the output of mini.ml: see the end of the file. *)

open Mini_ast

(* ---- Tokens ---- *)

type tok =
  | Kw of string  (** A reserved word. *)
  | Name of string
  | Number of int
  | Text of string  (** A string literal, unescaped. *)
  | Sym of string  (** Punctuation and operators. *)
  | Bad of char  (** A character that starts no token. *)

module Tok = struct
  type t = tok

  let equal = ( = )

  (* How a token is written: this is what "expected ..." lists show. *)
  let show = function
    | Kw s | Name s | Sym s -> s
    | Number n -> string_of_int n
    | Text s -> Printf.sprintf "%S" s
    | Bad c -> String.make 1 c
end

(* ---- The lexer ---- *)

module Lexer = struct
  open Parkour
  module L = (val Lexeme.make ~line_comments:[ "//" ] ())

  let reserved =
    [ "fn"; "let"; "if"; "else"; "while"; "return"; "true"; "false" ]

  (* Identifiers and keywords share a shape; the word decides which. *)
  let word =
    let start c = is_letter c || c = '_' in
    let+ w =
      consumed
        (satisfy "letter" start *> skip_while (fun c -> start c || is_digit c))
    in
    if List.mem w reserved then Kw w else Name w

  (* Longest match: two-character operators before their one-character
     prefixes. *)
  let symbol =
    let+ s =
      choice
        (List.map string [ "=="; "!="; "<="; ">="; "&&"; "||" ]
        @ List.map
            (fun c -> String.make 1 <$> char c)
            [
              '+';
              '-';
              '*';
              '/';
              '%';
              '<';
              '>';
              '!';
              '=';
              '(';
              ')';
              '{';
              '}';
              ',';
              ';';
            ])
    in
    Sym s

  (* A character no token starts with becomes a [Bad] token instead of a
     lexing error. Lexing carries on, so the parser still sees (and can
     report on) the rest of the program. [main] below turns each [Bad] token
     into an "unexpected character" diagnostic. An unterminated string, on the
     other hand, stops the lexer: where it would end is anyone's guess. *)
  let token =
    choice
      [
        word;
        (let+ n = natural in
         Number n);
        (let+ s = lookahead (char '"') *> cut *> string_literal in
         Text s);
        symbol;
        (let+ c = any_char in
         Bad c);
      ]

  let lex = tokenize ~skip:L.space token
end

(* ---- The parser: the same grammar as mini.ml, on tokens ---- *)

module T = Parkour.Tokens (Tok)
open T

let kw word = token (Kw word)
let sym s = token (Sym s)

(* [expect] picks a token apart: it extracts the payload of the tokens it
   accepts, and "identifier" is what a failure here is called. *)
let identifier = expect "identifier" (function Name s -> Some s | _ -> None)

(* Where a name is expected, say what is wrong with a reserved word. *)
let name =
  let reserved_word =
    let* word, span =
      hidden
        (spanned (expect "keyword" (function Kw w -> Some w | _ -> None)))
    in
    fail ~span
      ~notes:[ "reserved words cannot be used as names" ]
      (Printf.sprintf "'%s' is a reserved word" word)
  in
  identifier <|> reserved_word

(* ---- Expressions ---- *)

(* Hidden and cut, for the reasons given in mini.ml. *)
let op symbol f =
  let+ _ = hidden (sym symbol) <* cut in
  f symbol

let binary symbol = op symbol (fun s a b -> Binary (s, a, b))
let unary symbol = op symbol (fun s a -> Unary (s, a))

(* Tokens are matched whole, so no care is needed over "<" and "<=". *)
let operators =
  [
    [ Prefix (unary "-"); Prefix (unary "!") ];
    [
      Infix_left (binary "*"); Infix_left (binary "/"); Infix_left (binary "%");
    ];
    [ Infix_left (binary "+"); Infix_left (binary "-") ];
    [
      Infix_none (binary "<");
      Infix_none (binary "<=");
      Infix_none (binary ">");
      Infix_none (binary ">=");
    ];
    [ Infix_none (binary "=="); Infix_none (binary "!=") ];
    [ Infix_left (binary "&&") ];
    [ Infix_left (binary "||") ];
  ]

let parens item =
  between (sym "(" <* cut) (sym ")") (sep_by ~sep:(sym ",") item)

let expr =
  fix (fun expr ->
      let call_or_variable =
        let+ f = name and+ args = optional (parens expr) in
        match args with None -> Var f | Some args -> Call (f, args)
      in
      let grouped = between (sym "(" <* cut) (sym ")") expr in
      let term =
        choice ~label:"expression"
          [
            expect "number" (function Number n -> Some (Int n) | _ -> None);
            expect "string" (function Text s -> Some (Str s) | _ -> None);
            (let+ _ = kw "true" in
             Bool true);
            (let+ _ = kw "false" in
             Bool false);
            call_or_variable;
            grouped;
          ]
      in
      expression term operators)

(* ---- Statements ---- *)

let semicolon = or_insert (Sym ";") (sym ";") >>| ignore
let skip_statement = skip_past ~nested:(sym "{", sym "}") (sym ";")

let recovering statement =
  recover ~catch:`Any ~sync:skip_statement
    ~fallback:(fun _ -> Recovered)
    statement

let block statement =
  between ~or_insert:true (sym "{") (sym "}") (many (recovering statement))

let statement =
  fix (fun statement ->
      let block = block statement in
      let let_ =
        let+ () = kw "let" *> cut
        and+ x = name
        and+ () = sym "=" *> cut
        and+ e = expr
        and+ () = semicolon in
        Let (x, e)
      in
      let assign =
        let* x = name <* sym "=" in
        let+ e = cut *> expr and+ () = semicolon in
        Assign (x, e)
      in
      let else_branch =
        let else_if =
          let+ s = lookahead (kw "if") *> statement in
          [ s ]
        in
        optional (kw "else" *> cut *> (block <|> else_if))
      in
      let if_ =
        let+ () = kw "if" *> cut
        and+ c = expr
        and+ yes = block
        and+ no = else_branch in
        If (c, yes, Option.value no ~default:[])
      in
      let while_ =
        let+ () = kw "while" *> cut and+ c = expr and+ body = block in
        While (c, body)
      in
      let return_ =
        let+ () = kw "return" *> cut
        and+ e =
          choice
            [
              (let+ _ = sym ";" in
               None);
              (let+ e = expr and+ () = semicolon in
               Some e);
            ]
        in
        Return e
      in
      let expression_statement =
        let+ e = expr and+ () = semicolon in
        Expr e
      in
      choice ~label:"statement"
        [ let_; if_; while_; return_; assign; expression_statement ])

(* ---- Functions and the program ---- *)

let no_duplicates params =
  let rec check seen = function
    | [] -> return (List.rev_map fst seen)
    | (x, span) :: rest -> (
        match List.assoc_opt x seen with
        | Some first ->
            fail ~span
              ~labels:[ (first, "first defined here") ]
              (Printf.sprintf "duplicate parameter '%s'" x)
        | None -> check ((x, span) :: seen) rest)
  in
  check [] params

let header =
  recover ~catch:`Any
    ~sync:(skip_to (sym "{"))
    ~fallback:(fun _ -> ("?", []))
    (let+ f = name and+ params = parens (spanned name) >>= no_duplicates in
     (f, params))

let func =
  let+ () = kw "fn" *> cut
  and+ name, params = header
  and+ body = block statement in
  Fn { name; params; body }

let broken_function = skip_to ~nested:(sym "{", sym "}") (kw "fn")

let stray_brace =
  recover ~sync:(return ())
    ~fallback:(fun _ -> Stmt Recovered)
    (let* _, span = spanned (sym "}") in
     cut *> fail ~span "unmatched '}'")

let item =
  choice
    [
      recover ~sync:broken_function ~fallback:(fun _ -> Stmt Recovered) func;
      stray_brace;
      (let+ s = recovering statement in
       Stmt s);
    ]

let program = many item <* eof

(* ---- Running ---- *)

let start d = (Parkour.Diagnostic.span d).Parkour.Span.start

(* Lex, then parse. A [Bad] token is reported by the lexer's own diagnostic
   ("unexpected character"), and the parser's complaint about the same token
   ("expected ';', found '#'") is dropped as a duplicate. *)
let diagnose source =
  match Lexer.lex source with
  | Error diagnostics -> (None, diagnostics)
  | Ok tokens ->
      let bad =
        Array.to_list tokens
        |> List.filter_map (function
          | Bad c, span ->
              Some
                (Parkour.Diagnostic.make ~span
                   (Printf.sprintf "unexpected character '%c'" c))
          | _ -> None)
      in
      let result, parser_errors =
        parse_partial program (input ~source tokens)
      in
      (* The complaint is either on the token or, for a missing token,
         just before it with the token as a secondary label. *)
      let at_bad d =
        List.exists
          (fun b ->
            start b = start d
            || List.exists
                 (fun (span, _) -> span.Parkour.Span.start = start b)
                 (Parkour.Diagnostic.labels d))
          bad
      in
      let parser_errors = List.filter (fun d -> not (at_bad d)) parser_errors in
      ( result,
        List.stable_sort
          (fun a b -> compare (start a) (start b))
          (bad @ parser_errors) )

let run ~name ~full source =
  let result, diagnostics = diagnose source in
  let src = Parkour.Source.make ~name source in
  List.iter
    (fun d -> print_string (Parkour.Diagnostic.render src d))
    diagnostics;
  match result with
  | None -> print_endline "parsed: nothing"
  | Some program ->
      if diagnostics = [] || full then print_program program;
      print_endline (summary program)

let builtin = {|fn square(x) { return x * x; }
let n = square(7);
print(n);
|}

let () =
  match Array.to_list Sys.argv with
  | [ _; "--gallery" ] ->
      List.iter
        (fun (title, source) ->
          Printf.printf "=== %s ===\n" title;
          run ~name:"gallery.mini" ~full:false source)
        gallery
  | [ _; path ] ->
      run ~name:path ~full:true
        (In_channel.with_open_bin path In_channel.input_all)
  | _ -> run ~name:"builtin.mini" ~full:true builtin

(* Differences from mini.ml's gallery. Only one remains: the bad character.
   Here the lexer reports "unexpected character '#'" and the parser's own
   complaints at that token are dropped. In mini.ml the grammar sees the raw
   character and can only say "expected ';', found '#'" (and, since the
   statement is then skipped from the '#', a second error). Everything else,
   including the wording of the messages and which statements are recovered,
   is identical: both grammars have the same shape. *)
