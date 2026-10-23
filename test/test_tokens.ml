(* Token input: Tokens against Text, tokenize, and a small language parsed both
   ways. *)

module P = Parkour
module D = P.Diagnostic
module Prop = Parkour_test_support.Prop
module G = Parkour_test_support.Grammar
open QCheck2

(* Conformance: one-char tokens with 1-byte spans *)

module Char_token = struct
  type t = char

  let equal = Char.equal
  let show = String.make 1
end

module Chars = P.Tokens (Char_token)
module Compile_text = G.Compile (P)
module Compile_chars = G.Compile (Chars)

let chars_input s =
  Chars.input ~source:s
    (Array.init (String.length s) (fun i ->
         (s.[i], P.Span.make ~start:i ~stop:(i + 1))))

type 'a outcome = Raised | V of 'a * int | E of D.t

let outcome parse_prefix p input =
  match parse_prefix p input with
  | Ok (v, pos) -> V (v, pos)
  | Error (d :: _) -> E d
  | Error [] -> failwith "no diagnostic"
  | exception P.Grammar_error _ -> Raised

(* Everything that does not depend on how the unexpected input is quoted: the
   token run [found] covers is a word for Text and one token for Tokens. *)
let same_outcome a b =
  match (a, b) with
  | Raised, Raised -> true
  | V (v, n), V (w, m) -> v = w && n = m
  | E d, E e ->
      D.expected d = D.expected e
      && (D.span d).start = (D.span e).start
      && D.context d = D.context e
      &&
      if D.found d = None then D.message d = D.message e && D.span d = D.span e
      else D.found e <> None
  | _ -> false

(* A single character is quoted the same by both. *)
let same_found_for_chars s = function
  | E d
    when D.expected d <> []
         && D.found d <> None
         && (D.span d).start < String.length s ->
      D.found d = Some (Printf.sprintf "'%c'" s.[(D.span d).start])
  | _ -> true

let gen_case ~cuts = Gen.pair (G.gen ~cuts) G.gen_input

let conformance cuts =
  Prop.test
    ~name:(Printf.sprintf "Tokens over chars = Text (cuts=%b)" cuts)
    ~print:(fun (g, s) -> G.to_string g ^ " on " ^ String.escaped s)
    (gen_case ~cuts)
    (fun (g, s) ->
      let pt = Compile_text.compile g and pk = Compile_chars.compile g in
      let prefix =
        ( outcome P.parse_prefix pt s,
          outcome Chars.parse_prefix pk (chars_input s) )
      in
      let whole =
        ( outcome (fun p i -> Result.map (fun v -> (v, 0)) (P.parse p i)) pt s,
          outcome
            (fun p i -> Result.map (fun v -> (v, 0)) (Chars.parse p i))
            pk (chars_input s) )
      in
      List.for_all (fun (a, b) -> same_outcome a b) [ prefix; whole ]
      && same_found_for_chars s (snd prefix)
      && same_found_for_chars s (snd whole))

(* tokenize *)

type word = Word of string | C | Cc

let lexer =
  let open P in
  tokenize
    ~skip:(skip_while (fun c -> c = ' '))
    (choice
       [
         map
           (fun w -> Word w)
           (take_while1 "word" (fun c -> c = 'a' || c = 'b'));
         map (fun _ -> Cc) (string "cc");
         map (fun _ -> C) (char 'c');
       ])

let gen_lex_input =
  Gen.(
    string_size
      ~gen:
        (oneof_weighted
           [
             (3, oneof_list [ 'a'; 'b'; 'c' ]); (2, return ' '); (1, return '?');
           ])
      (int_bound 20))

let tokenize_prop =
  Prop.test ~name:"tokenize: spans tile the input; bad char is a diagnostic"
    ~print:String.escaped gen_lex_input (fun s ->
      match (lexer s, String.index_opt s '?') with
      | Ok toks, None ->
          let n = String.length s in
          let spaces a b =
            a <= b && String.for_all (fun c -> c = ' ') (String.sub s a (b - a))
          in
          let ok = ref true and at = ref 0 in
          Array.iter
            (fun (t, (sp : P.Span.t)) ->
              let text = String.sub s sp.start (sp.stop - sp.start) in
              let text_matches =
                match t with
                | Word w -> w = text
                | C -> text = "c"
                | Cc -> text = "cc"
              in
              ok :=
                !ok && sp.stop > sp.start && spaces !at sp.start && text_matches;
              at := sp.stop)
            toks;
          !ok && spaces !at n
      | Error [ d ], Some i -> (D.span d).start = i
      | _ -> false)

(* The same small language, parsed from text and from tokens *)

module type PRIM = sig
  include P.S

  val kw : string -> unit t
  val sym : string -> unit t
  val ident : string t
  val int : int t
end

module Lang (L : PRIM) = struct
  open L

  let bin op a b = Printf.sprintf "(%s %s %s)" a op b

  let expr =
    fix (fun expr ->
        let atom =
          choice
            [ map string_of_int int; ident; between (sym "(") (sym ")") expr ]
        in
        let op s = sym s *> return (bin s) in
        let add = chainl1 (chainl1 atom (op "*")) (op "+") in
        let cmp =
          let* a = add in
          option a
            (let* () = sym "==" in
             map (bin "==" a) add)
        in
        let let_ =
          let* () = kw "let" in
          let* () = cut in
          let* x = ident in
          let* () = sym "<-" in
          let* e = expr in
          let* () = kw "in" in
          map (fun b -> Printf.sprintf "(let %s %s %s)" x e b) expr
        in
        let_ <|> cmp)

  let program = expr <* eof
end

module Text_lang = Lang (struct
  include P

  let l =
    P.Lexeme.make ~ident_start:P.is_letter ~ident_char:P.is_letter
      ~reserved:[ "let"; "in" ] ()

  module L = (val l)

  let kw w = map ignore (L.keyword w)
  let sym s = map ignore (L.symbol s)
  let ident = L.identifier
  let int = L.lexeme natural
end)

type tok = Kw of string | Sym of string | Id of string | Int of int

module Tok = struct
  type t = tok

  let equal = ( = )
  let show = function Kw s | Sym s | Id s -> s | Int n -> string_of_int n
end

module T = P.Tokens (Tok)

module Token_lang = Lang (struct
  include T

  let kw w = map ignore (token (Kw w))
  let sym s = map ignore (token (Sym s))
  let ident = expect "identifier" (function Id s -> Some s | _ -> None)
  let int = expect "integer" (function Int n -> Some n | _ -> None)
end)

let lang_lexer =
  let open P in
  tokenize ~skip:(skip_while is_space)
    (choice
       [
         map
           (fun w -> if w = "let" || w = "in" then Kw w else Id w)
           (take_while1 "identifier" is_letter);
         map (fun n -> Int n) natural;
         map (fun s -> Sym s) (string "==" <|> string "<-");
         map (fun c -> Sym (String.make 1 c)) (one_of "+*()");
       ])

let gen_source =
  let open Gen in
  let atom = oneof_list [ [ "x" ]; [ "yz" ]; [ "1" ]; [ "23" ] ] in
  let expr =
    sized_size (int_bound 4)
      (fix (fun self n ->
           if n = 0 then atom
           else
             let sub = self (n - 1) in
             oneof_weighted
               [
                 (2, atom);
                 ( 4,
                   map3
                     (fun op a b -> a @ (op :: b))
                     (oneof_list [ "+"; "*"; "==" ])
                     sub sub );
                 (1, map (fun a -> ("(" :: a) @ [ ")" ]) sub);
                 ( 1,
                   map3
                     (fun x a b -> ("let" :: x :: "<-" :: a) @ ("in" :: b))
                     (oneof_list [ "x"; "yz" ])
                     sub sub );
               ]))
  in
  let sep = oneof_list [ " "; " "; ""; "\n" ] in
  let* toks = expr in
  let* toks =
    (* corrupt: drop, duplicate or swap a piece *)
    let* extra =
      oneof_list [ "let"; "in"; "=="; "<-"; "("; ")"; "+"; "x"; "1" ]
    in
    let* k = int_bound 6 in
    let* i = int_bound (max 1 (List.length toks)) in
    return
      (match k with
      | 0 -> List.filteri (fun j _ -> j <> i) toks
      | 1 ->
          List.concat
            (List.mapi (fun j t -> if j = i then [ t; t ] else [ t ]) toks)
      | 2 -> (
          match List.filteri (fun j _ -> j >= i) toks with
          | a :: b :: _ ->
              List.mapi
                (fun j t -> if j = i then b else if j = i + 1 then a else t)
                toks
          | _ -> toks)
      | 3 ->
          List.concat
            (List.mapi (fun j t -> if j = i then [ extra; t ] else [ t ]) toks)
      | _ -> toks)
  in
  let* seps = list_size (return (List.length toks)) sep in
  return (String.concat "" (List.map2 ( ^ ) toks seps))

let language_agreement =
  Prop.test ~name:"language: text grammar and tokenize + Tokens agree"
    ~print:String.escaped gen_source (fun s ->
      let text = P.parse Text_lang.program s in
      match (text, lang_lexer s) with
      | _, Error _ -> false
      | text, Ok toks -> (
          match (text, T.parse Token_lang.program (T.input ~source:s toks)) with
          | Ok a, Ok b -> a = b
          | Error (d :: _), Error (e :: _) ->
              (D.span d).start = (D.span e).start && D.expected d = D.expected e
          | _ -> false))

let lexer_error_comes_first =
  Prop.test ~name:"language: a bad character is a lexer error at it"
    ~print:(fun (s, _) -> String.escaped s)
    Gen.(
      let* s = gen_source in
      let* i = int_bound (String.length s) in
      return (String.sub s 0 i ^ "?" ^ String.sub s i (String.length s - i), i))
    (fun (s, i) ->
      (match lang_lexer s with
        | Error [ d ] -> (D.span d).start <= i
        | _ -> false)
      &&
      match P.parse Text_lang.program s with
      | Error (d :: _) -> (D.span d).start <= String.index s '?'
      | _ -> false)

let stack_safety =
  Prop.test ~count:1 ~name:"100000 tokens: lexing and parsing are stack-safe"
    Gen.unit (fun () ->
      let n = 100_000 in
      let s = String.concat " " (List.init n string_of_int) in
      match lang_lexer s with
      | Error _ -> false
      | Ok toks -> (
          let int =
            T.expect "integer" (function Int k -> Some k | _ -> None)
          in
          match T.parse (T.many int) (T.input ~source:s toks) with
          | Ok l -> List.length l = n
          | Error _ -> false))

(* tokens, with recovery *)

type num = Num of int | Bad

let start_of d = (D.span d).start

let diag severity start =
  D.make ~severity ~span:(P.Span.make ~start ~stop:start) "emitted"

(* Space-separated words: a number token (a warning is emitted for multiples of
   7), or a run of letters, which is a recovered lexer error. *)
let recovering_lexer =
  let open P in
  let number =
    let* start = position in
    let* n = natural in
    (if n mod 7 = 0 then emit (diag `Warning start) else return ())
    *> return (Num n)
  in
  let junk =
    recover ~catch:`Any
      ~sync:(skip_while (fun c -> c <> ' '))
      ~fallback:(fun _ -> Bad)
      (fail "bad token")
  in
  tokens ~skip:(skip_while (fun c -> c = ' ')) (number <|> junk)

let tokens_recovering =
  Prop.test
    ~name:"tokens with parse_partial returns the warnings and recovered errors"
    ~print:Print.(list string)
    Gen.(
      list_size (int_bound 8)
        (oneof
           [
             map string_of_int (int_bound 30);
             string_size ~gen:(char_range 'a' 'z') (int_range 1 3);
           ]))
    (fun items ->
      let is_number s = s.[0] <= '9' in
      let input = String.concat " " items in
      let placed =
        let at = ref 0 in
        List.map
          (fun item ->
            let start = !at in
            at := start + String.length item + 1;
            (item, start))
          items
      in
      let expected =
        List.map
          (fun (item, start) ->
            ( (if is_number item then Num (int_of_string item) else Bad),
              P.Span.make ~start ~stop:(start + String.length item) ))
          placed
      in
      let at keep =
        List.filter_map
          (fun (item, start) -> if keep item then Some start else None)
          placed
      in
      let value, log = P.parse_partial recovering_lexer input in
      let starts f = List.map start_of (List.filter f log) in
      value = Some (Array.of_list expected)
      && starts (fun d -> not (D.is_error d))
         = at (fun s -> is_number s && int_of_string s mod 7 = 0)
      && starts D.is_error = at (fun s -> not (is_number s)))

(* [token] returns the input's token, not its argument. *)

module Ci_token = struct
  type t = string

  let equal a b = String.lowercase_ascii a = String.lowercase_ascii b
  let show = Fun.id
end

module Ci = P.Tokens (Ci_token)

let token_returns_input =
  Prop.test ~name:"token returns the token found in the input"
    ~print:Print.(pair string string)
    Gen.(
      let word = string_size ~gen:(char_range 'a' 'c') (int_range 1 3) in
      let* w = word in
      let* upper = bool in
      return (w, if upper then String.uppercase_ascii w else w))
    (fun (arg, found) ->
      let input =
        Ci.input ~source:found [| (found, P.Span.make ~start:0 ~stop:1) |]
      in
      Ci.parse (Ci.token arg) input = Ok found)

(* Insertion points *)

module Trivia = (val P.Lexeme.make ~line_comments:[ "#" ] ())

let gen_trivia =
  Gen.(
    map (String.concat "")
      (list_size (int_bound 3)
         (oneof_list [ " "; "\n"; "  "; "# note\n"; "\t" ])))

type ins = X | Semi | Y

module Ins_tok = struct
  type t = ins

  let equal = ( = )
  let show = function X -> "x" | Semi -> ";" | Y -> "y"
end

module Ins = P.Tokens (Ins_tok)

let ins_lexer =
  let open P in
  tokenize ~skip:Trivia.space
    (choice
       [
         map (fun _ -> X) (char 'x');
         map (fun _ -> Semi) (char ';');
         map (fun _ -> Y) (char 'y');
       ])

let lexed s = Result.to_option (ins_lexer s)

let insertion_point =
  Prop.test ~name:"or_insert reports at the end of the previous token"
    ~print:(fun (t, next) ->
      Printf.sprintf "x%S%s" t (if next then "y" else ""))
    Gen.(pair gen_trivia bool)
    (fun (trivia, next) ->
      let s = "x" ^ trivia ^ if next then "y" else "" in
      let text =
        P.(
          Trivia.symbol "x"
          *> or_insert ";" (Trivia.symbol ";")
          *> many (Trivia.symbol "y"))
      in
      let toks =
        Ins.(token X *> or_insert Semi (token Semi) *> many (token Y))
      in
      match (P.parse_partial text s, lexed s) with
      | (Some _, [ d ]), Some tokens -> (
          match Ins.parse_partial toks (Ins.input ~source:s tokens) with
          | Some _, [ e ] ->
              let found_at = String.length trivia + 1 in
              let expected_labels =
                if next && trivia <> "" then
                  [ (P.Span.make ~start:found_at ~stop:(found_at + 1), "") ]
                else []
              in
              D.span d = P.Span.point 1
              && D.span e = D.span d
              && D.message d = D.message e
              && D.labels d = expected_labels
              && D.labels e = D.labels d
          | _ -> false)
      | _ -> false)

let between_or_insert =
  Prop.test ~name:"between ~or_insert always passes; one error iff unclosed"
    ~print:(fun (t1, t2, closed) ->
      Printf.sprintf "(%S x %S closed=%b" t1 t2 closed)
    Gen.(triple gen_trivia gen_trivia bool)
    (fun (t1, t2, closed) ->
      let s = "(" ^ t1 ^ "x" ^ t2 ^ if closed then ")" else "" in
      let p =
        P.between ~or_insert:true (Trivia.symbol "(") (Trivia.symbol ")")
          (Trivia.symbol "x")
      in
      match P.parse_partial p s with
      | Some "x", [] -> closed
      | Some "x", [ d ] ->
          (not closed)
          && D.span d = P.Span.point (2 + String.length t1)
          && D.labels d
             = [
                 ( P.Span.make ~start:0 ~stop:(1 + String.length t1),
                   "opened here" );
               ]
      | _ -> false)

let () =
  Prop.run "tokens"
    [
      conformance false;
      conformance true;
      tokenize_prop;
      language_agreement;
      lexer_error_comes_first;
      tokens_recovering;
      token_returns_input;
      stack_safety;
      insertion_point;
      between_or_insert;
    ]
