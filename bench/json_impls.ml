(* Every JSON implementation, as [string -> (Json_ast.t, string) result]. The
   error string is the library's own message (with a position when it gives
   one), which [main --errors] prints. *)

open Json_ast

let utf8 buf code =
  if Uchar.is_valid code then Buffer.add_utf_8_uchar buf (Uchar.of_int code)
  else failwith "invalid \\u escape"

let combine hi lo = 0x10000 + ((hi - 0xD800) lsl 10) + (lo - 0xDC00)
let is_high hi = hi >= 0xD800 && hi < 0xDC00
let is_low lo = lo >= 0xDC00 && lo < 0xE000

let is_number_char = function
  | '0' .. '9' | '-' | '+' | '.' | 'e' | 'E' -> true
  | _ -> false

(* ---- Parkour: examples/json.ml, as users see it ---- *)

module Parkour_json = struct
  open Parkour

  let ws = whitespace
  let lexeme p = p <* ws
  let sym c = lexeme (char c)
  let str = lexeme string_literal
  let num = lexeme number

  let rec value =
    lazy
      (choice ~label:"value"
         [
           lexeme (string "null") *> return Null;
           lexeme (string "true") *> return (Bool true);
           lexeme (string "false") *> return (Bool false);
           (num >>| fun n -> Number n);
           (str >>| fun s -> String s);
           force array;
           force obj;
         ])

  and array =
    lazy
      (context "array"
         ( between (sym '[') (sym ']')
             (cut *> sep_by ~sep:(sym ',') (force value))
         >>| fun items -> Array items ))

  and obj =
    lazy
      (let member =
         let+ key = str <* sym ':' and+ v = force value in
         (key, v)
       in
       context "object"
         ( between (sym '{') (sym '}') (cut *> sep_by ~sep:(sym ',') member)
         >>| fun members -> Object members ))

  let json = ws *> force value

  let parse src =
    match parse json src with
    | Ok v -> Ok v
    | Error ds ->
        let source = Source.make ~name:"input" src in
        Error
          (String.concat "\n"
             (List.map
                (fun d -> Format.asprintf "%a" (Diagnostic.pp_located source) d)
                ds))
end

(* ---- Angstrom, in the style of its RFC 7159 example ---- *)

module Angstrom_json = struct
  open Angstrom

  let ws = skip_while (function ' ' | '\t' | '\n' | '\r' -> true | _ -> false)
  let lexeme p = p <* ws
  let plain c = c <> '"' && c <> '\\' && c >= ' '
  let hex4 = take 4 >>| fun h -> int_of_string ("0x" ^ h)

  let escape buf =
    any_char >>= function
    | '"' ->
        Buffer.add_char buf '"';
        return ()
    | '\\' ->
        Buffer.add_char buf '\\';
        return ()
    | '/' ->
        Buffer.add_char buf '/';
        return ()
    | 'b' ->
        Buffer.add_char buf '\b';
        return ()
    | 'f' ->
        Buffer.add_char buf '\012';
        return ()
    | 'n' ->
        Buffer.add_char buf '\n';
        return ()
    | 'r' ->
        Buffer.add_char buf '\r';
        return ()
    | 't' ->
        Buffer.add_char buf '\t';
        return ()
    | 'u' -> (
        hex4 >>= fun hi ->
        if is_high hi then
          string "\\u" *> hex4 >>= fun lo ->
          if is_low lo then return (utf8 buf (combine hi lo))
          else fail "unpaired surrogate"
        else try return (utf8 buf hi) with Failure m -> fail m)
    | _ -> fail "invalid escape"

  (* Strings without escapes are one slice; escapes switch to a buffer. *)
  let string_lit =
    char '"' *> take_while plain >>= fun first ->
    any_char >>= function
    | '"' -> return first
    | '\\' ->
        let buf = Buffer.create 32 in
        Buffer.add_string buf first;
        let rec loop () =
          escape buf *> take_while plain >>= fun s ->
          Buffer.add_string buf s;
          any_char >>= function
          | '"' -> return (Buffer.contents buf)
          | '\\' -> loop ()
          | _ -> fail "invalid character in string"
        in
        loop ()
    | _ -> fail "invalid character in string"

  let number =
    take_while1 is_number_char >>= fun s ->
    match float_of_string_opt s with
    | Some f -> return f
    | None -> fail "invalid number"

  let json =
    fix (fun value ->
        let array =
          char '[' *> ws *> sep_by (lexeme (char ',')) value <* char ']'
          >>| fun l -> Array l
        in
        let member =
          lexeme string_lit >>= fun k ->
          lexeme (char ':') *> value >>| fun v -> (k, v)
        in
        let obj =
          char '{' *> ws *> sep_by (lexeme (char ',')) member <* char '}'
          >>| fun l -> Object l
        in
        lexeme
          ( peek_char_fail >>= function
            | '[' -> array
            | '{' -> obj
            | '"' -> string_lit >>| fun s -> String s
            | 'n' -> string "null" *> return Null
            | 't' -> string "true" *> return (Bool true)
            | 'f' -> string "false" *> return (Bool false)
            | _ -> number >>| fun n -> Number n ))

  let parse src = parse_string ~consume:All (ws *> json) src
end

(* ---- MParser ---- *)

module Mparser_json = struct
  open MParser

  let ws =
    skip_many_chars
      (satisfy (function ' ' | '\t' | '\n' | '\r' -> true | _ -> false))

  let lexeme p = p << ws
  let plain c = c <> '"' && c <> '\\' && c >= ' '

  let hex4 =
    count 4 hex_digit |>> fun l ->
    int_of_string ("0x" ^ String.of_seq (List.to_seq l))

  let escape =
    any_char >>= function
    | '"' -> return "\""
    | '\\' -> return "\\"
    | '/' -> return "/"
    | 'b' -> return "\b"
    | 'f' -> return "\012"
    | 'n' -> return "\n"
    | 'r' -> return "\r"
    | 't' -> return "\t"
    | 'u' ->
        hex4 >>= fun hi ->
        let code =
          if is_high hi then
            string "\\u" >> hex4 >>= fun lo ->
            if is_low lo then return (combine hi lo) else zero
          else return hi
        in
        code >>= fun c ->
        let buf = Buffer.create 4 in
        if Uchar.is_valid c then (
          utf8 buf c;
          return (Buffer.contents buf))
        else zero
    | _ -> zero

  let chunk = many1_satisfy plain <|> (char '\\' >> escape)
  let string_lit = char '"' >> many chunk << char '"' |>> String.concat ""

  let number =
    many1_satisfy is_number_char >>= fun s ->
    match float_of_string_opt s with Some f -> return f | None -> zero

  let rec value s =
    (choice
       [
         string "null" >>$ Null;
         string "true" >>$ Bool true;
         string "false" >>$ Bool false;
         (string_lit |>> fun s -> String s);
         (number |>> fun n -> Number n);
         array;
         obj;
       ]
    << ws)
      s

  and array s =
    ( char '[' >> ws >> sep_by value (lexeme (char ',')) << char ']' |>> fun l ->
      Array l )
      s

  and obj s =
    let member =
      lexeme string_lit >>= fun k ->
      lexeme (char ':') >> value |>> fun v -> (k, v)
    in
    ( char '{' >> ws >> sep_by member (lexeme (char ',')) << char '}'
    |>> fun l -> Object l )
      s

  let json = ws >> value << eof

  let parse src =
    match parse_string json src () with
    | Success v -> Ok v
    | Failed (msg, _) -> Error msg
end

(* ---- Yojson, converted to the shared tree (conversion is timed) ---- *)

module Yojson_json = struct
  let rec convert : Yojson.Basic.t -> Json_ast.t = function
    | `Null -> Null
    | `Bool b -> Bool b
    | `Int i -> Number (float_of_int i)
    | `Float f -> Number f
    | `String s -> String s
    | `List l -> Array (List.map convert l)
    | `Assoc l -> Object (List.map (fun (k, v) -> (k, convert v)) l)

  let parse src =
    match Yojson.Basic.from_string src with
    | v -> Ok (convert v)
    | exception Yojson.Json_error m -> Error m
end

(* ---- ocamllex + menhir ---- *)

module Menhir_json = struct
  let parse src =
    let lexbuf = Lexing.from_string src in
    let where () =
      let off = min lexbuf.Lexing.lex_curr_pos (String.length src) in
      let line = ref 1 and bol = ref 0 in
      String.iteri
        (fun i c ->
          if i < off && c = '\n' then (
            incr line;
            bol := i + 1))
        src;
      Printf.sprintf "%d:%d" !line (off - !bol + 1)
    in
    match Json_parser.main Json_lexer.token lexbuf with
    | v -> Ok v
    | exception Json_lexer.Error m -> Error (where () ^ ": " ^ m)
    | exception Json_parser.Error -> Error (where () ^ ": syntax error")
end

(* New Parkour variants (say, one using [dispatch]) are extra rows here. *)
let all : (string * (string -> (Json_ast.t, string) result)) list =
  [
    ("parkour", Parkour_json.parse);
    ("angstrom", Angstrom_json.parse);
    ("mparser", Mparser_json.parse);
    ("yojson+convert", Yojson_json.parse);
    ("ocamllex+menhir", Menhir_json.parse);
  ]
