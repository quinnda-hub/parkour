(* A CSV parser (RFC 4180 plus the usual extras) that reports every problem in
   a file in one run and keeps the rows it could read.

   Run on a file:             dune exec examples/csv.exe -- data.csv
   Run on a built-in sample:  dune exec examples/csv.exe
   Print the error gallery:   dune exec examples/csv.exe -- --gallery

   Records end with LF or CRLF (the last newline is optional); a UTF-8 byte
   order mark at the start is skipped; the delimiter is a parameter. A field is
   either bare text or a quoted string where [""] stands for a quote and
   newlines are allowed. The first record is the header.

   Each record is parsed with [recover], so one broken row is reported and
   skipped. Rows with the wrong number of fields are reported with [emit]
   without stopping the parse. *)

open Parkour

type table = { header : string list; rows : string list list; skipped : int }

let bom = string "\xEF\xBB\xBF"

(* A field is quoted or bare. [quoted] already does the hard work: [""] inside,
   newlines allowed, and errors that point at the opening quote. *)
let field ~delim =
  let bare c = c <> delim && c <> '"' && c <> '\r' && c <> '\n' in
  let quoted_field =
    let* text, span = spanned (quoted ~doubled:true ~multiline:true '"') in
    (* A closing quote must be followed by the end of the field. *)
    let* next = peek in
    match next with
    | Some c when c <> delim && c <> '\r' && c <> '\n' ->
        fail
          ~labels:[ (span, "this field ends here") ]
          (Printf.sprintf "unexpected %s after the closing quote"
             (Expected.to_string (Literal (String.make 1 c))))
    | _ -> return text
  in
  let bare_field =
    (* A quote in the middle of bare text is almost always a mistake. *)
    let* text = take_while bare in
    let* stray = peek in
    match stray with
    | Some '"' ->
        fail "unexpected quote in unquoted field"
          ~notes:[ "to include a quote, wrap the whole field in quotes" ]
    | _ -> return text
  in
  quoted_field <|> bare_field

(* One record, without its line ending. *)
let record ~delim = spanned (sep_by1 ~sep:(char delim) (field ~delim))

(* Recovery: a record with a syntax error is skipped up to the next physical
   line. This is simple and predictable, with one trade-off: if the bad record
   contains a quoted field spanning several lines, the lines after the break
   are parsed as records of their own and may be reported too. Tracking quotes
   from the error position would guess wrong as often as right, since we do
   not know whether the error is inside or outside a quoted field. [None] is
   the placeholder for a skipped record. *)
let recovering row =
  recover ~catch:`Any ~sync:skip_line
    ~fallback:(fun _ -> None)
    (row <* (newline <|> eof) >>= Fun.id)

(* A row is kept only if it has as many fields as the header. Otherwise the
   mismatch is reported (pointing back at the header) and parsing goes on. *)
let check_width (header, header_span) (fields, span) =
  let count n = if n = 1 then "1 field" else Printf.sprintf "%d fields" n in
  let expected = List.length header in
  if List.length fields = expected then return (Some fields)
  else
    emit
      (Diagnostic.make ~span
         ~labels:[ (header_span, "the header has " ^ count expected) ]
         (Printf.sprintf "expected %s, found %d" (count expected)
            (List.length fields)))
    *> return None

let table ~delim =
  let row = record ~delim in
  let blank_lines = skip_many newline in
  let header =
    peek >>= function
    | None -> fail "expected a header row, found end of input"
    | Some _ -> recovering (row >>| fun r -> return (Some r))
  in
  let* _ = optional bom in
  let* header = header <* blank_lines in
  let check =
    match header with
    | Some h -> check_width h
    | None -> fun (_, _) -> return None
  in
  let+ rows =
    many (not_followed_by eof *> recovering (row >>| check) <* blank_lines)
  in
  let kept = List.filter_map Fun.id rows in
  {
    header = Option.fold header ~none:[] ~some:fst;
    rows = kept;
    skipped = List.length rows - List.length kept;
  }

(* Everything below is the demo driver. *)

(* An unterminated quote swallows the rest of the file, which is rarely what
   the author meant, so say where the quote went. *)
let explain d =
  if Diagnostic.message d = "unterminated string" then
    Diagnostic.add_note
      "the quoted field runs to the end of the input; check for a missing \
       closing quote"
      d
  else d

let report ?(name = "input.csv") ~delim input =
  let result, diagnostics = parse_partial (table ~delim <* eof) input in
  let diagnostics = List.map explain diagnostics in
  let source = Source.make ~name input in
  List.iter (fun d -> print_string (Diagnostic.render source d)) diagnostics;
  (result, diagnostics)

let sample =
  "name,role,city\nAda,\"Engineer, lead\",London\nGrace,Admiral,\"New\nYork\"\n"

let gallery =
  [
    ( "valid: commas, newlines and doubled quotes in quoted fields",
      ',',
      "title,quote,tags\n\
       Hamlet,\"To be, or not to be\",\"drama\n\
       classic\"\n\
       Dune,\"He said \"\"fear is the mind-killer\"\"\",scifi\n" );
    ( "CRLF line endings and a byte order mark",
      ',',
      "\xEF\xBB\xBFid,name\r\n1,Ada\r\n2,Grace\r\n" );
    ( "ragged rows",
      ',',
      "id,name,email,team\n\
       1,Ada,ada@example.com,core\n\
       2,Grace,grace@example.com\n\
       3,Linus,linus@example.com,kernel\n\
       4,Margaret\n" );
    ( "stray quote in an unquoted field",
      ',',
      "id,name\n1,Ada\n2,Gra\"ce\n3,Linus\n" );
    ("garbage after a closing quote", ',', "id,name\n1,\"Ada\"x\n2,Grace\n");
    ( "unterminated quoted field",
      ',',
      "id,name,note\n\
       1,Ada,fine\n\
       2,Grace,\"never closed\n\
       3,Linus,fine\n\
       4,Margaret,fine\n" );
    ("semicolon delimiter", ';', "id;name;note\n1;Ada;\"a;b\"\n2;Grace\n");
    ("empty input", ',', "");
  ]

let summary = function
  | Some t ->
      let n = List.length t.rows in
      Printf.printf "parsed: %d row%s (%d skipped)\n" n
        (if n = 1 then "" else "s")
        t.skipped
  | None -> print_endline "parsed: nothing"

let () =
  match Array.to_list Sys.argv with
  | [ _; "--gallery" ] ->
      List.iter
        (fun (title, delim, input) ->
          Printf.printf "=== %s ===\n" title;
          let result, _ = report ~name:"gallery.csv" ~delim input in
          summary result)
        gallery
  | [ _; path ] ->
      let input = In_channel.with_open_bin path In_channel.input_all in
      let result, _ = report ~name:path ~delim:',' input in
      summary result
  | _ ->
      let result, _ = report ~name:"sample.csv" ~delim:',' sample in
      summary result;
      Option.iter
        (fun t ->
          List.iter
            (fun r -> print_endline (String.concat " | " r))
            (t.header :: t.rows))
        result
