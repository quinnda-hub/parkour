(* The expression grammar: integers, [+ - * /] (left associative), unary
   minus (tighter than [*]), parentheses and whitespace. *)

open Expr_ast

(* ---- Input: about 1 MB, deterministic, with nested parentheses ---- *)

let generate ?(size = 1_000_000) () =
  let st = Random.State.make [| 42 |] in
  let buf = Buffer.create (size + 4096) in
  let sp () = if Random.State.int st 4 = 0 then Buffer.add_char buf ' ' in
  let rec operand depth =
    (* a unary minus is never directly followed by another one *)
    if Random.State.int st 8 = 0 then (
      Buffer.add_char buf '-';
      sp ());
    if depth < 30 && Random.State.int st 5 = 0 then (
      Buffer.add_char buf '(';
      sp ();
      sum depth (1 + Random.State.int st 6);
      Buffer.add_char buf ')')
    else Buffer.add_string buf (string_of_int (Random.State.int st 1_000_000));
    sp ()
  and sum depth n =
    operand (depth + 1);
    for _ = 2 to n do
      Buffer.add_char buf "+-*/".[Random.State.int st 4];
      sp ();
      operand (depth + 1)
    done
  in
  while Buffer.length buf < size do
    if Buffer.length buf > 0 then Buffer.add_string buf " + ";
    sum 0 (1 + Random.State.int st 8)
  done;
  Buffer.contents buf

(* ---- Parkour ---- *)

module Parkour_expr = struct
  open Parkour
  module L = (val Lexeme.make ())

  let op c f = L.symbol (String.make 1 c) >>| fun _ -> f
  let bin c = op c (fun a b -> Bin (c, a, b))

  let expr =
    fix (fun expr ->
        let term =
          L.lexeme natural
          >>| (fun n -> Int n)
          <|> between (L.symbol "(") (L.symbol ")") expr
        in
        expression term
          [
            [ Prefix (op '-' (fun e -> Neg e)) ];
            [ Infix_left (bin '*'); Infix_left (bin '/') ];
            [ Infix_left (bin '+'); Infix_left (bin '-') ];
          ])

  let parse src =
    match parse (L.space *> expr) src with
    | Ok v -> Ok v
    | Error ds ->
        let source = Source.make ~name:"input" src in
        Error
          (String.concat "\n"
             (List.map
                (fun d -> Format.asprintf "%a" (Diagnostic.pp_located source) d)
                ds))
end

(* ---- Angstrom ---- *)

module Angstrom_expr = struct
  open Angstrom

  let ws = skip_while (function ' ' | '\t' | '\n' | '\r' -> true | _ -> false)
  let lexeme p = p <* ws
  let op c f = lexeme (char c) *> return (fun a b -> f c a b)
  let bin c a b = Bin (c, a, b)

  (* From Angstrom's README. *)
  let chainl1 e op =
    let rec go acc = lift2 (fun f x -> f acc x) op e >>= go <|> return acc in
    e >>= go

  let expr =
    fix (fun expr ->
        let int =
          lexeme (take_while1 (function '0' .. '9' -> true | _ -> false))
          >>| fun s -> Int (int_of_string s)
        in
        let atom = int <|> (lexeme (char '(') *> expr <* lexeme (char ')')) in
        let unary =
          fix (fun unary ->
              atom <|> (lexeme (char '-') *> unary >>| fun e -> Neg e))
        in
        let mul = chainl1 unary (op '*' bin <|> op '/' bin) in
        chainl1 mul (op '+' bin <|> op '-' bin))

  let parse src = parse_string ~consume:All (ws *> expr) src
end

(* ---- MParser ---- *)

module Mparser_expr = struct
  open MParser

  let ws =
    skip_many_chars
      (satisfy (function ' ' | '\t' | '\n' | '\r' -> true | _ -> false))

  let lexeme p = p << ws
  let op c f = lexeme (char c) >>$ f
  let bin c = op c (fun a b -> Bin (c, a, b))
  let infix c = Infix (bin c, Assoc_left)

  let rec expr s =
    expression
      [
        [ Prefix (op '-' (fun e -> Neg e)) ];
        [ infix '*'; infix '/' ];
        [ infix '+'; infix '-' ];
      ]
      term s

  and term s =
    (lexeme (many1_satisfy (function '0' .. '9' -> true | _ -> false))
    |>> (fun d -> Int (int_of_string d))
    <|> (lexeme (char '(') >> expr << lexeme (char ')')))
      s

  let parse src =
    match parse_string (ws >> expr << eof) src () with
    | Success v -> Ok v
    | Failed (msg, _) -> Error msg
end

(* ---- ocamllex + menhir ---- *)

module Menhir_expr = struct
  let parse src =
    match Expr_parser.main Expr_lexer.token (Lexing.from_string src) with
    | v -> Ok v
    | exception Expr_lexer.Error m -> Error m
    | exception Expr_parser.Error -> Error "syntax error"
end

let all : (string * (string -> (Expr_ast.t, string) result)) list =
  [
    ("parkour", Parkour_expr.parse);
    ("angstrom", Angstrom_expr.parse);
    ("mparser", Mparser_expr.parse);
    ("ocamllex+menhir", Menhir_expr.parse);
  ]
