(* A JSON parser written with Parkour.

   Run it on a file:          dune exec examples/json.exe -- data.json
   Or on built-in samples:    dune exec examples/json.exe
   Print the error gallery:   dune exec examples/json.exe -- --gallery

   It shows the main techniques: mutually recursive parsers with
   [let rec ... = lazy] and [force], [hidden] whitespace, [cut] after an
   opening delimiter so errors inside it are not retried as something else,
   [between] so an unclosed bracket points back to where it was opened, and
   [context] for "while parsing ..." information. Strings and numbers come from
   the library: [string_literal] handles every JSON escape, including \u escapes
   and surrogate pairs, and [number] reports out-of-range values. *)

open Parkour

type json =
  | Null
  | Bool of bool
  | Number of float
  | String of string
  | Array of json list
  | Object of (string * json) list

(* Every token eats the whitespace after it, so the grammar never mentions
   whitespace again; [whitespace] is [hidden], so it never shows up in
   "expected ..." lists either. *)
let ws = whitespace
let lexeme p = p <* ws
let sym c = lexeme (char c)
let str = lexeme string_literal
let num = lexeme number

let rec value =
  lazy
    (* [label] collapses the seven ways a value can start into one word. *)
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

(* [cut] right after the opening bracket: once we see [ or {, nothing else can
   be meant, so errors inside are reported as they are. [between] adds the
   "opened here" label when the closing bracket is missing. *)
and array =
  lazy
    (context "array"
       ( between (sym '[') (sym ']') (cut *> sep_by ~sep:(sym ',') (force value))
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

(* Diagnostics are plain data; [Diagnostic.render] turns them into a report
   with a source excerpt. *)
let report ?max_depth name src =
  match parse ?max_depth json src with
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

(* The gallery: one broken input per mistake, each with its own title. Valid
   inputs print a one-line summary. The last nesting case uses a small
   [max_depth] so the output stays short. *)
let gallery =
  [
    ( "valid document",
      {|{"name": "parkour", "tags": ["a", "b"], "n": 1.5e3}|},
      None );
    ("valid empty containers", {|{"a": [], "b": {}}|}, None);
    ("trailing comma in an array", {|[1, 2, 3,]|}, None);
    ("trailing comma in an object", {|{"a": 1, "b": 2,}|}, None);
    ("single-quoted string", {|{"name": 'parkour'}|}, None);
    ("unquoted key", {|{name: "parkour"}|}, None);
    ("NaN", {|[1, NaN]|}, None);
    ("a // comment", "{\n  \"a\": 1 // the answer\n}", None);
    ("missing comma between members", "{\n  \"a\": 1\n  \"b\": 2\n}", None);
    ("missing colon", {|{"a" 1}|}, None);
    ("unclosed [ spanning several lines", "[\n  1,\n  2,\n  3\n", None);
    ("unclosed { before more input", "{\n  \"a\": [1, 2]\n  \"b\": 3\n", None);
    ("unterminated string", {|{"name": "parkour}|}, None);
    ("invalid escape", {|["tab\q"]|}, None);
    ("number out of range", {|[1, 1e999]|}, None);
    ("nesting deeper than the limit (max_depth 8)", String.make 20 '[', Some 8);
    ("empty input", "", None);
    ("trailing garbage after the value", {|{"a": 1} {"b": 2}|}, None);
  ]

let run_gallery () =
  List.iter
    (fun (title, src, max_depth) ->
      Printf.printf "=== %s ===\n" title;
      match parse ?max_depth json src with
      | Ok _ -> print_endline "ok: parsed"
      | Error diagnostics ->
          let source = Source.make ~name:"gallery.json" src in
          List.iter
            (fun d -> print_string (Diagnostic.render source d))
            diagnostics)
    gallery

let () =
  match Sys.argv with
  | [| _; "--gallery" |] -> run_gallery ()
  | [| _; file |] ->
      let ic = open_in_bin file in
      let src = really_input_string ic (in_channel_length ic) in
      close_in ic;
      report file src
  | _ ->
      List.iteri
        (fun i src -> report (Printf.sprintf "sample%d.json" (i + 1)) src)
        samples
