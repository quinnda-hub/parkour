(* An INI-like configuration format that reports every problem it finds in one
   run and still returns what it could read.

     # comment
     [section]
     key = 42        (integers, "strings" and booleans)

   Each line is parsed with [recover]: a broken line is recorded as a
   diagnostic and skipped, and parsing resumes on the next line. *)

open Parkour
module L = (val Lexeme.make ~line_comments:[ "#" ] ~newlines:false ())

type value = Int of int | Str of string | Bool of bool

(* Blank and comment-only lines. *)
let blank = skip_many (L.space *> newline)

(* The end of a line, then any blank lines. *)
let end_of_line = newline <|> eof <?> "end of line" <* blank

let value =
  choice ~label:"value"
    [
      (let+ n = L.lexeme integer in
       Int n);
      (let+ s = L.lexeme string_literal in
       Str s);
      (let+ _ = L.keyword "true" in
       Bool true);
      (let+ _ = L.keyword "false" in
       Bool false);
    ]

(* [None] is the placeholder for a line that could not be read. The [cut]
   after [=] makes a later failure on the line recoverable like any other;
   [`Any] also recovers from a line that does not even start with a key. A
   section header ends the section, so it is excluded first. *)
let entry =
  not_followed_by (char '[')
  *> recover ~catch:`Any ~sync:(skip_line *> blank)
       ~fallback:(fun _ -> None)
       (let+ key, span = spanned L.identifier
        and+ () = L.symbol "=" *> cut
        and+ value = value <* end_of_line in
        Some (key, value, span))

(* A key defined twice in a section is only worth a warning. *)
let warn_duplicates entries =
  let seen = Hashtbl.create 8 in
  List.fold_left
    (fun acc -> function
      | Some (key, _, span) when Hashtbl.mem seen key ->
          let warning =
            Diagnostic.make ~severity:`Warning ~span
              (Printf.sprintf "duplicate key %S" key)
          in
          acc *> emit warning
      | Some (key, _, _) ->
          Hashtbl.add seen key ();
          acc
      | None -> acc)
    (return ()) entries

let header =
  recover ~sync:(skip_line *> blank)
    ~fallback:(fun _ -> "?")
    (L.symbol "[" *> cut
    *> let+ name = L.lexeme (take_while1 "section name" is_alnum)
       and+ _ = or_insert "]" (L.symbol "]") in
       name)
  <* end_of_line

let section =
  context "section"
    (let* name = header in
     let* entries = many entry in
     let+ () = warn_duplicates entries in
     (name, List.filter_map (Option.map (fun (k, v, _) -> (k, v))) entries))

let file = blank *> many section <* eof

let sample =
  {|# demo
[server]
host = "localhost"
port = 80x
debug = true
debug = false
name =

[paths
root = "/srv"
= 5
timeout = 99999999999999999999

[empty]
|}

let show_value = function
  | Int n -> string_of_int n
  | Str s -> Printf.sprintf "%S" s
  | Bool b -> string_of_bool b

let () =
  let source = Source.make ~name:"sample.conf" sample in
  let result, diagnostics = parse_partial file sample in
  List.iter (fun d -> print_string (Diagnostic.render source d)) diagnostics;
  Printf.printf "\n%d diagnostics; parsed so far:\n" (List.length diagnostics);
  List.iter
    (fun (name, entries) ->
      Printf.printf "[%s]\n" name;
      List.iter
        (fun (k, v) -> Printf.printf "  %s = %s\n" k (show_value v))
        entries)
    (Option.value result ~default:[])
