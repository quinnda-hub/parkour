(* A JSON parser written with Parkour.

   Run it on a file:          dune exec examples/json.exe -- data.json
   Or on built-in samples:    dune exec examples/json.exe

   It shows the main techniques: mutually recursive parsers with
   [let rec ... = lazy] and [force], [hidden] whitespace, [cut] after an
   opening delimiter so errors inside it are not retried as something else,
   [context] for "while parsing ..." information, and [validate] for checks
   that underline a whole token. String escapes are limited to the simple
   ones (no \u escapes) to keep the example short. *)

open Parkour

type json =
  | Null
  | Bool of bool
  | Number of float
  | String of string
  | Array of json list
  | Object of (string * json) list

let is_space = function ' ' | '\n' | '\t' | '\r' -> true | _ -> false
let ws = hidden (skip_while is_space)
let lexeme p = p <* ws
let sym c = lexeme (char c)

let escape =
  char '\\' *> cut
  *> choice ~label:"escape sequence"
       [
         char '"' *> return '"';
         char '\\' *> return '\\';
         char '/' *> return '/';
         char 'n' *> return '\n';
         char 't' *> return '\t';
         char 'r' *> return '\r';
       ]

let string_literal =
  let chunk = take_while1 "character" (fun c -> c <> '"' && c <> '\\') in
  let piece = chunk <|> (escape >>| String.make 1) in
  lexeme (char '"' *> cut *> many piece <* char '"' >>| String.concat "")
  <?> "string"

let number =
  lexeme
    (take_while1 "number" (function
      | '0' .. '9' | '-' | '+' | '.' | 'e' | 'E' -> true
      | _ -> false))
  |> validate (fun s ->
      match float_of_string_opt s with
      | Some f -> Ok f
      | None -> Error ("invalid number " ^ s))

let rec value =
  lazy
    (choice ~label:"value"
       [
         lexeme (string "null") *> return Null;
         lexeme (string "true") *> return (Bool true);
         lexeme (string "false") *> return (Bool false);
         (number >>| fun n -> Number n);
         (string_literal >>| fun s -> String s);
         force array;
         force obj;
       ])

and array =
  lazy
    (context "array"
       ( sym '[' *> cut *> sep_by ~sep:(sym ',') (force value) <* sym ']'
       >>| fun items -> Array items ))

and obj =
  lazy
    (let member =
       let+ key = string_literal <* sym ':' and+ v = force value in
       (key, v)
     in
     context "object"
       ( sym '{' *> cut *> sep_by ~sep:(sym ',') member <* sym '}'
       >>| fun members -> Object members ))

let json = ws *> force value

(* Diagnostics are plain data; [Diagnostic.render] turns them into a report
   with a source excerpt. *)
let report name src =
  match parse json src with
  | Ok _ -> Printf.printf "%s: ok\n" name
  | Error diagnostics ->
      let source = Source.make ~name src in
      List.iter (fun d -> print_string (Diagnostic.render source d)) diagnostics

let samples =
  [
    {|{"a": [1, 2, {"b": null}], "c": true}|};
    {|{"a": [1, 2,]}|};
    {|{"a" 1}|};
    {|[1, 2|};
    {|{"a": tru}|};
    {|[1.2.3]|};
    {|{"a": [1, 2] "b": 3}|};
    {|["tab\q"]|};
    "";
  ]

let () =
  match Sys.argv with
  | [| _; file |] ->
      let ic = open_in_bin file in
      let src = really_input_string ic (in_channel_length ic) in
      close_in ic;
      report file src
  | _ ->
      List.iteri
        (fun i src -> report (Printf.sprintf "sample%d.json" (i + 1)) src)
        samples
