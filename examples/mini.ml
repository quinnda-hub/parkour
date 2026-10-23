(* Mini, a small C-like language, parsed directly from text. It is the
   showcase for error recovery: one run reports every mistake in a program
   and still returns the parts that were fine.

   Run the built-in program: dune exec examples/mini.exe
   Parse a file:             dune exec examples/mini.exe -- program.mini
   Print the error gallery:  dune exec examples/mini.exe -- --gallery

   The ideas, in the order they appear below:
   - [cut] right after the word that identifies a construct ([let], [if], ...),
     so that a mistake after it is that construct's mistake;
   - [recover] around each statement: the error is recorded, input is skipped
     up to the end of the statement, and parsing goes on;
   - [or_insert] for a missing ';': one error, and the statement is read as if
     the semicolon were there;
   - [between] for blocks and parentheses: an unclosed one points back at its
     opener;
   - [fail ~span ~labels] for a check the grammar cannot express. *)

open Parkour
open Mini_ast

let reserved = [ "fn"; "let"; "if"; "else"; "while"; "return"; "true"; "false" ]

module L = (val Lexeme.make ~line_comments:[ "//" ] ~reserved ())

(* ---- Names ---- *)

(* [L.identifier] refuses reserved words, which gives the unhelpful "expected
   identifier, found "while"". Wherever a name is expected we also recognise
   a reserved word, to say what is wrong. [hidden] keeps the keywords out of
   the "expected ..." list, which stays "expected identifier". *)
let name =
  let reserved_word =
    let* word, span = hidden (spanned (choice (List.map L.keyword reserved))) in
    fail ~span
      ~notes:[ "reserved words cannot be used as names" ]
      (Printf.sprintf "'%s' is a reserved word" word)
  in
  L.identifier <|> reserved_word

(* ---- Expressions ---- *)

(* Operators are [hidden]: after an operand the error should say what is
   missing from the statement (say, ';'), not list every operator that could
   have continued the expression. The [cut] after an operator commits to it,
   so that in [n - ;] the error is the missing operand ("expected
   expression, found ';'"), not a ';' that was expected instead of the '-'. *)
let op symbol f =
  let+ _ = hidden (L.symbol symbol) <* cut in
  f symbol

let binary symbol = op symbol (fun s a b -> Binary (s, a, b))
let unary symbol = op symbol (fun s a -> Unary (s, a))

(* Tightest level first. "<=" must come before "<" as they share a prefix. The
   comparisons are not associative: [a < b < c] is an error. *)
let operators =
  [
    [ Prefix (unary "-"); Prefix (unary "!") ];
    [
      Infix_left (binary "*"); Infix_left (binary "/"); Infix_left (binary "%");
    ];
    [ Infix_left (binary "+"); Infix_left (binary "-") ];
    [
      Infix_none (binary "<=");
      Infix_none (binary ">=");
      Infix_none (binary "<");
      Infix_none (binary ">");
    ];
    [ Infix_none (binary "=="); Infix_none (binary "!=") ];
    [ Infix_left (binary "&&") ];
    [ Infix_left (binary "||") ];
  ]

(* Parenthesised, comma-separated things: call arguments and parameters.
   The [cut] after "(" commits to the list; [between] makes a missing ")"
   point back at the "(". *)
let parens item =
  between (L.symbol "(" <* cut) (L.symbol ")") (sep_by ~sep:(L.symbol ",") item)

let expr =
  fix (fun expr ->
      let call_or_variable =
        let+ f = name and+ args = optional (parens expr) in
        match args with None -> Var f | Some args -> Call (f, args)
      in
      let grouped = between (L.symbol "(" <* cut) (L.symbol ")") expr in
      let term =
        choice ~label:"expression"
          [
            (let+ n = L.lexeme natural in
             Int n);
            (let+ s = L.lexeme string_literal in
             Str s);
            (let+ _ = L.keyword "true" in
             Bool true);
            (let+ _ = L.keyword "false" in
             Bool false);
            call_or_variable;
            grouped;
          ]
      in
      expression term operators)

(* ---- Statements ---- *)

(* A missing ';' is recorded as an error, and parsing goes on as if it were
   there, so the next statement is not reported as broken too. *)
let semicolon = or_insert ";" (L.symbol ";") >>| ignore

(* Where to resume after a broken statement: just after the next ';', not
   counting those inside braces. An unmatched "}" also stops the skipping (just
   before it), so that it still closes the enclosing block. *)
let skip_statement =
  skip_past ~nested:(L.symbol "{", L.symbol "}") (L.symbol ";")

(* Statements are recovered wherever they appear in a list, so a mistake in
   one costs only that statement. We catch [`Any] failure here and not only
   committed ones: expression statements start with no keyword to [cut] after,
   and nothing else could be tried in place of a statement anyway. At the "}"
   that ends a block the statement fails without skipping anything; recovery
   then gives way and the block closes normally. *)
let recovering statement =
  recover ~catch:`Any ~sync:skip_statement
    ~fallback:(fun _ -> Recovered)
    statement

(* A block is a list of statements between braces. *)
let block statement =
  between ~or_insert:true (L.symbol "{") (L.symbol "}")
    (many (recovering statement))

let statement =
  fix (fun statement ->
      let block = block statement in
      let let_ =
        let+ () = L.keyword "let" *> cut
        and+ x = name
        and+ () = L.symbol "=" *> cut
        and+ e = expr
        and+ () = semicolon in
        Let (x, e)
      in
      (* [x = e;] and [e;] both start with an expression. Only after the "="
         do we know it is an assignment. "==" is a different token. *)
      let assign =
        let* x = name <* L.lexeme (string "=" <* not_followed_by (char '=')) in
        let+ e = cut *> expr and+ () = semicolon in
        Assign (x, e)
      in
      let else_branch =
        (* [else if ...] is an else branch holding one [if] statement. *)
        let else_if =
          let+ s = lookahead (L.keyword "if") *> statement in
          [ s ]
        in
        optional (L.keyword "else" *> cut *> (block <|> else_if))
      in
      let if_ =
        let+ () = L.keyword "if" *> cut
        and+ c = expr
        and+ yes = block
        and+ no = else_branch in
        If (c, yes, Option.value no ~default:[])
      in
      let while_ =
        let+ () = L.keyword "while" *> cut and+ c = expr and+ body = block in
        While (c, body)
      in
      (* The value is optional. It is a [choice] rather than [optional expr]
         because [optional] would swallow a failure inside the expression and
         then complain about the ';' instead. *)
      let return_ =
        let+ () = L.keyword "return" *> cut
        and+ e =
          choice
            [
              (let+ _ = L.symbol ";" in
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

(* The duplicate check cannot be written in the grammar, so it runs on the
   parsed list. [fail ~span] underlines the repeated name and [~labels] shows
   the first one. *)
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

(* [fn name(params)]. A mistake here is recovered by skipping to the body's
   "{" so the body is still checked. *)
let header =
  recover ~catch:`Any
    ~sync:(skip_to (L.symbol "{"))
    ~fallback:(fun _ -> ("?", []))
    (let+ f = name and+ params = parens (spanned name) >>= no_duplicates in
     (f, params))

let func =
  let+ () = L.keyword "fn" *> cut
  and+ name, params = header
  and+ body = block statement in
  Fn { name; params; body }

(* A mistake in a function that its body could not recover from, say an
   unclosed brace, skips to the next function. Nested braces are skipped as
   a unit. *)
let broken_function =
  skip_to ~nested:(L.symbol "{", L.symbol "}") (L.keyword "fn")

(* A "}" that closes nothing. [recover]'s sync is [return ()]: the error is
   reported on the brace, which is then simply dropped. *)
let stray_brace =
  recover ~sync:(return ())
    ~fallback:(fun _ -> Stmt Recovered)
    (let* _, span = spanned (L.symbol "}") in
     cut *> fail ~span "unmatched '}'")

let item =
  choice
    [
      recover ~sync:broken_function ~fallback:(fun _ -> Stmt Recovered) func;
      stray_brace;
      (let+ s = recovering statement in
       Stmt s);
    ]

let program = L.space *> many item <* eof

(* ---- Running ---- *)

(* [full]: print the program even if there were errors, with <error>
   placeholders for the statements that were skipped. *)
let run ~name ~full source =
  let result, diagnostics = parse_partial program source in
  let src = Source.make ~name source in
  List.iter (fun d -> print_string (Diagnostic.render src d)) diagnostics;
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
