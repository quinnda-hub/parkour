(* Command-line front end for the JSON grammar in json_parser.ml.

   Run it on a file:          dune exec examples/json.exe -- data.json
   Or on built-in samples:    dune exec examples/json.exe
   Print the error gallery:   dune exec examples/json.exe -- --gallery *)

open Parkour
open Json_parser

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
    ("leading zero in a number", {|[1, 01]|}, None);
    ("raw tab inside a string", "[\"a\tb\"]", None);
    ("invalid UTF-8 inside a string", "[\"a\xffb\"]", None);
    ("form feed between tokens", "[1,\x0c2]", None);
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
