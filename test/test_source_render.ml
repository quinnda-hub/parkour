open Parkour
module Prop = Parkour_test_support.Prop
open QCheck2

(* Generators *)

let fragments =
  [|
    "a";
    "b";
    "x";
    " ";
    "\n";
    "\r\n";
    "\r";
    "\t";
    "\xc3\xa9";
    "\xe6\x97\xa5";
    "\xf0\x9f\x99\x82";
    "\xff";
    "\x80";
    "\xe6\x97";
    "\x01";
    "\x7f";
    "\x1b";
    "\xcc\x81";
  |]

let newline_heavy = [| "\n"; "\r\n"; "a"; "\n"; "\xe6\x97\xa5" |]

let gen_text =
  let frag = Gen.oneof_array in
  Gen.(
    map (String.concat "")
      (list_size (Prop.bound 30) (oneof [ frag fragments; frag newline_heavy ])))

let print_text = Printf.sprintf "%S"

(* A naive decoder: (start, length, code point or -1) for each character. *)
let chars s =
  let rec go i acc =
    if i >= String.length s then List.rev acc
    else
      let d = String.get_utf_8_uchar s i in
      if Uchar.utf_decode_is_valid d then
        go
          (i + Uchar.utf_decode_length d)
          (( i,
             Uchar.utf_decode_length d,
             Uchar.to_int (Uchar.utf_decode_uchar d) )
          :: acc)
      else go (i + 1) ((i, 1, -1) :: acc)
  in
  go 0 []

(* Offsets at character boundaries. *)
let boundaries s = String.length s :: List.map (fun (i, _, _) -> i) (chars s)

(* Boundaries other than the LF of a CRLF. *)
let positions s =
  List.filter
    (fun o ->
      not (o > 0 && o < String.length s && s.[o - 1] = '\r' && s.[o] = '\n'))
    (boundaries s)

let line_start s o =
  if o = 0 then 0
  else
    match String.rindex_from_opt s (o - 1) '\n' with
    | Some k -> k + 1
    | None -> 0

(* Source properties *)

let roundtrip =
  Prop.test
    ~name:
      "Source: offset inverts position at character boundaries (except the LF \
       of a CRLF)" ~print:print_text gen_text (fun s ->
      let src = Source.make s in
      List.for_all
        (fun o ->
          let p = Source.position src o in
          p.offset = o && Source.offset src ~line:p.line ~column:p.column = o)
        (positions s))

let line_tiling =
  Prop.test ~name:"Source: lines tile the text, split at LF or CRLF only"
    ~print:print_text gen_text (fun s ->
      let src = Source.make s in
      let n = String.length s in
      let lf = List.length (String.split_on_char '\n' s) - 1 in
      let count = Source.line_count src in
      count = lf + 1
      && List.for_all
           (fun l ->
             let sp = Source.line_span src l in
             let content = String.sub s sp.start (Span.length sp) in
             (not (String.contains content '\n'))
             &&
             if l = count then sp.stop = n
             else
               let next = (Source.line_span src (l + 1)).start in
               let term = String.sub s sp.stop (next - sp.stop) in
               term = "\n" || term = "\r\n")
           (List.init count (fun i -> i + 1))
      && (Source.line_span src 1).start = 0)

let line_end s ls =
  match String.index_from_opt s ls '\n' with
  | None -> String.length s
  | Some e -> if e > ls && s.[e - 1] = '\r' then e - 1 else e

let counting =
  Prop.test
    ~name:
      "Source: line, column, char_offset, utf16_column match a naive decoder \
       at every offset" ~print:print_text gen_text (fun s ->
      let src = Source.make s in
      List.for_all
        (fun o ->
          let whole = List.filter (fun (i, l, _) -> i + l <= o) (chars s) in
          let ls = line_start s o in
          let stop = min o (line_end s ls) in
          let same_line =
            List.filter (fun (i, l, _) -> i >= ls && i + l <= stop) (chars s)
          in
          let u16 =
            List.fold_left
              (fun a (_, _, c) -> a + if c >= 0x10000 then 2 else 1)
              0 same_line
          in
          let line = ref 1 in
          String.iteri (fun i c -> if i < o && c = '\n' then incr line) s;
          let p = Source.position src o in
          Source.char_offset src o = List.length whole
          && Source.utf16_column src o = u16
          && p.line = !line
          && p.column = List.length same_line + 1)
        (List.init (String.length s + 1) Fun.id))

(* Render properties *)

let gen_render_case =
  Gen.(
    gen_text >>= fun s ->
    let n = String.length s in
    int_bound (n + 3) >>= fun a ->
    int_bound 12 >>= fun l ->
    let start = a and stop = a + if l > 6 then 40 else l in
    list_size (int_bound 5)
      (map
         (fun k ->
           (Printf.sprintf "ctx%d" k, Span.make ~start:(min k n) ~stop:n))
         (int_bound 9))
    >>= fun context ->
    list_size (int_bound 3)
      (oneof
         [
           map (fun l -> Expected.Label l) (oneof_list [ "integer"; "name" ]);
           return Expected.End_of_input;
           return (Expected.Literal ";");
         ])
    >>= fun expected ->
    oneof_list [ 1; 2; 3; 7; 20; 100 ] >>= fun max_width ->
    oneof_list [ 1; 4; 8 ] >>= fun tab_width ->
    int_bound 4 >>= fun max_context ->
    oneof_list [ `Line_column; `Char_offset; `Byte_offset ] >>= fun location ->
    bool >>= fun warn ->
    oneof_list [ None; Some "f.txt" ] >>= fun name ->
    return
      ( (s, name),
        Diagnostic.make
          ~severity:(if warn then `Warning else `Error)
          ~expected ~context ~span:(Span.make ~start ~stop) "msg",
        (location, tab_width, max_width, max_context) ))

let print_render_case ((s, _), d, (_, tw, mw, mc)) =
  Printf.sprintf "%S span %s tab %d width %d ctx %d" s
    (Format.asprintf "%a" Span.pp (Diagnostic.span d))
    tw mw mc

let render_both ((s, name), d, (location, tab_width, max_width, max_context)) =
  let src = Source.make ?name s in
  let r color =
    Diagnostic.render ~location ~tab_width ~max_width ~max_context ~color src d
  in
  (r false, r true)

let strip_escapes s =
  let b = Buffer.create (String.length s) in
  let n = String.length s in
  let rec go i =
    if i < n then
      if s.[i] = '\027' then
        let j = String.index_from s i 'm' in
        go (j + 1)
      else (
        Buffer.add_char b s.[i];
        go (i + 1))
  in
  go 0;
  Buffer.contents b

let render_shape =
  Prop.test
    ~name:
      "render: never raises; plain output is clean UTF-8 with one final newline"
    ~print:print_render_case gen_render_case (fun (((s, _), d, _) as c) ->
      let plain, _ = render_both c in
      let n = String.length plain in
      let lines = String.split_on_char '\n' plain in
      let sp = Diagnostic.span d in
      let contains sub =
        let m = String.length sub in
        let rec go i =
          i + m <= n && (String.sub plain i m = sub || go (i + 1))
        in
        go 0
      in
      let start_line =
        if sp.stop <= String.length s then
          Some (Source.position (Source.make s) sp.start).line
        else None
      in
      (not (String.contains plain '\027'))
      && String.is_valid_utf_8 plain
      && n > 0
      && plain.[n - 1] = '\n'
      && (n < 2 || plain.[n - 2] <> '\n')
      && List.for_all (fun l -> l = "" || l.[String.length l - 1] <> ' ') lines
      &&
      match start_line with
      | None -> true
      | Some l -> contains (string_of_int l ^ " |"))

let render_colour =
  Prop.test ~name:"render: stripping colour codes gives the plain output"
    ~print:print_render_case gen_render_case (fun c ->
      let plain, coloured = render_both c in
      strip_escapes coloured = plain)

let gen_ascii_line =
  Gen.(
    string_size
      ~gen:(oneof [ char_range 'a' 'z'; return ' '; return ';' ])
      (int_bound 60)
    >>= fun s ->
    int_bound (String.length s) >>= fun a ->
    map (fun l -> (s, a, min l (String.length s - a))) (int_bound 8))

let caret_alignment =
  Prop.test
    ~name:
      "render: carets start under the span's column and cover it minus \
       trailing whitespace (ASCII line)"
    ~print:Print.(triple print_text int int)
    gen_ascii_line
    (fun (s, a, l) ->
      let d = Diagnostic.make ~span:(Span.make ~start:a ~stop:(a + l)) "m" in
      let rec trimmed l =
        if l > 0 && s.[a + l - 1] = ' ' then trimmed (l - 1) else l
      in
      let l = if trimmed l = 0 then l else trimmed l in
      let out = Diagnostic.render (Source.make s) d in
      match String.split_on_char '\n' out with
      | _ :: _ :: _ :: _ :: caret :: _ ->
          String.index caret '^' = 4 + a
          && String.length (String.concat "" (String.split_on_char '^' caret))
             = String.length caret - max 1 l
      | _ -> false)

(* The renderer's tables, written out again here: control and bidirectional
   characters are replaced, zero-width and wide characters change the columns.
   Every interval is probed at its ends and one scalar beyond each. *)
let c1_ranges = [ (0x80, 0x9F) ]

(* The Unicode Bidi_Control property (PropList.txt), written from the standard
   and deliberately independent of lib/: U+061C, U+200E-200F, U+202A-202E,
   U+2066-2069. *)
let bidi_controls =
  [ 0x061C; 0x200E; 0x200F ]
  @ [ 0x202A; 0x202B; 0x202C; 0x202D; 0x202E ]
  @ [ 0x2066; 0x2067; 0x2068; 0x2069 ]

let bidi_ranges = List.map (fun c -> (c, c)) bidi_controls
let odd_ranges = c1_ranges @ bidi_ranges

let zero_ranges =
  [
    (0x0300, 0x036F);
    (0x1AB0, 0x1AFF);
    (0x1DC0, 0x1DFF);
    (0x20D0, 0x20FF);
    (0xFE20, 0xFE2F);
    (0x200B, 0x200F);
    (* U+200E and U+200F are bidirectional controls, so shown as U+FFFD (see
       [odd_ranges], which takes precedence); the zero-width rule never reaches
       them. *)
    (0xFE00, 0xFE0F);
    (0xE0100, 0xE01EF);
  ]

let wide_ranges =
  [
    (0x1100, 0x115F);
    (0x2E80, 0x303E);
    (0x3041, 0x33FF);
    (0x3400, 0x4DBF);
    (0x4E00, 0x9FFF);
    (0xA000, 0xA4CF);
    (0xAC00, 0xD7A3);
    (0xF900, 0xFAFF);
    (0xFE30, 0xFE4F);
    (0xFF00, 0xFF60);
    (0xFFE0, 0xFFE6);
    (0x1F300, 0x1F64F);
    (0x1F900, 0x1F9FF);
    (0x20000, 0x2FFFD);
    (0x30000, 0x3FFFD);
  ]

let within ranges c = List.exists (fun (lo, hi) -> lo <= c && c <= hi) ranges

let probes =
  let around (lo, hi) = [ lo - 1; lo; hi; hi + 1 ] in
  let all =
    List.concat_map around (odd_ranges @ zero_ranges @ wide_ranges)
    @ [ 0x1F; 0x20; 0x7E; 0x7F; 0x80; 0xFF; 0x2FF; 0x300 ]
  in
  List.sort_uniq compare
    (List.filter (fun c -> Uchar.is_valid c && c <> 0x0A && c <> 0x0D) all)

let utf8 c =
  let b = Buffer.create 4 in
  Buffer.add_utf_8_uchar b (Uchar.of_int c);
  Buffer.contents b

let displayed c =
  if within odd_ranges c then "\xef\xbf\xbd"
  else if c < 0x20 then utf8 (0x2400 + c)
  else if c = 0x7F then utf8 0x2421
  else utf8 c

let display_width c =
  if c < 0x20 || c = 0x7F || within odd_ranges c || c < 0x300 then 1
  else if within zero_ranges c then 0
  else if within wide_ranges c then 2
  else 1

let unicode_cells =
  Prop.test
    ~name:
      "render: a character is shown sanitised and takes its display width, at \
       the ends of every control and width interval"
    ~print:(fun before ->
      String.concat " " (List.map (Printf.sprintf "U+%04X") before))
    Gen.(list_size (int_bound 3) (oneof_list probes))
    (fun before ->
      let prefix = String.concat "" (List.map utf8 before) in
      let col = List.fold_left (fun a x -> a + display_width x) 0 before in
      let check c =
        let text = prefix ^ utf8 c ^ "b" in
        let shown =
          String.concat "" (List.map displayed (before @ [ c ])) ^ "b"
        in
        let mark a b =
          let d = Diagnostic.make ~span:(Span.make ~start:a ~stop:b) "m" in
          match
            String.split_on_char '\n'
              (Diagnostic.render ~max_width:1000 (Source.make text) d)
          with
          | _ :: _ :: _ :: content :: marker :: _ -> Some (content, marker)
          | _ -> None
        in
        let n = String.length prefix in
        let stop = n + String.length (utf8 c) in
        (* the character itself, then the 'b' after it *)
        mark n stop
        = Some
            ( "1 | " ^ shown,
              "  | " ^ String.make col ' '
              ^ String.make (max 1 (display_width c)) '^' )
        && mark stop (stop + 1)
           = Some
               ( "1 | " ^ shown,
                 "  | " ^ String.make (col + display_width c) ' ' ^ "^" )
      in
      List.for_all check probes)

let huge_line =
  Prop.once ~name:"render: one error in a 10 MB single line is cheap" (fun () ->
      let n = 10_000_000 in
      let s = String.make n 'a' in
      let out =
        Diagnostic.render (Source.make s)
          (Diagnostic.make ~span:(Span.make ~start:(n - 5) ~stop:(n - 1)) "m")
      in
      String.length out < 1000)

(* Secondary labels and notes *)

(* ASCII text with newlines; a primary and 1-3 secondary spans, each with a
   unique text, and 0-2 notes. Short lines and long spans, so that spans often
   cross several lines (more than four lines, too) and overlap. *)
let gen_labelled =
  let open Gen in
  let line =
    string_size ~gen:(oneof [ char_range 'a' 'z'; return ' ' ]) (Prop.bound 10)
  in
  let* text = map (String.concat "\n") (list_size (Prop.range 1 10) line) in
  let n = String.length text in
  let span =
    let* a = int_bound n in
    let* l = oneof [ int_bound (min 8 (n - a)); int_bound (min 70 (n - a)) ] in
    return (Span.make ~start:a ~stop:(a + l))
  in
  let* primary = span in
  let* secondary = list_size (int_range 1 3) span in
  let* notes = int_bound 2 in
  return (text, primary, secondary, notes)

let print_labelled (text, p, sec, notes) =
  Printf.sprintf "%S primary %d-%d secondary %s notes %d" text p.Span.start
    p.stop
    (String.concat ","
       (List.map
          (fun (s : Span.t) -> Printf.sprintf "%d-%d" s.start s.stop)
          sec))
    notes

let labelled_diagnostic (text, primary, secondary, notes) =
  let d =
    Diagnostic.make ~label:"P0"
      ~labels:
        (List.mapi (fun i sp -> (sp, Printf.sprintf "S%d" (i + 1))) secondary)
      ~notes:(List.init notes (fun i -> Printf.sprintf "N%d" (i + 1)))
      ~span:primary "message"
  in
  (Source.make text, d)

let render_labelled c =
  let src, d = labelled_diagnostic c in
  Diagnostic.render src d

let occurrences ~sub s =
  let n = String.length sub in
  let rec go i acc =
    if i + n > String.length s then acc
    else go (i + 1) (if String.sub s i n = sub then acc + 1 else acc)
  in
  go 0 0

let labelled_shape =
  Prop.test
    ~name:
      "render with labels: no trailing spaces, one final newline, each text \
       once"
    ~print:print_labelled gen_labelled (fun ((_, _, secondary, notes) as c) ->
      let out = render_labelled c in
      let lines = String.split_on_char '\n' out in
      let tags =
        ("P0" :: List.mapi (fun i _ -> Printf.sprintf "S%d" (i + 1)) secondary)
        @ List.init notes (fun i -> Printf.sprintf "N%d" (i + 1))
      in
      out <> ""
      && out.[String.length out - 1] = '\n'
      && List.nth lines (List.length lines - 1) = ""
      && List.nth lines (List.length lines - 2) <> ""
      && List.for_all (fun l -> l = "" || l.[String.length l - 1] <> ' ') lines
      && List.for_all (fun tag -> occurrences ~sub:tag out = 1) tags)

(* The model of an annotation: its lines, after trimming trailing blanks. *)
let model text (sp : Span.t) =
  let n = String.length text in
  let start = min sp.start n and stop = min sp.stop n in
  let stop =
    let rec trim e =
      if e > start then
        if String.contains " \t\n\r\x0c" text.[e - 1] then trim (e - 1) else e
      else stop
    in
    trim stop
  in
  let line_of o =
    1
    + List.length
        (List.filter (( = ) '\n')
           (List.of_seq (String.to_seq (String.sub text 0 o))))
  in
  let first = line_of start in
  let last = if stop > start then line_of (stop - 1) else first in
  (start, stop, first, last)

let shown_lines first last =
  if last - first + 1 > 4 then [ first; first + 1; last ]
  else List.init (last - first + 1) (fun k -> first + k)

let rows_of out = List.filter (fun l -> l <> "") (String.split_on_char '\n' out)

let first_nonblank l =
  let rec go i = if i < String.length l && l.[i] = ' ' then go (i + 1) else i in
  go 0

(* The lines drawn for a set of annotations, and those of them that are the
   filler of a one-line gap. *)
let layout text spans =
  let shown =
    List.sort_uniq compare
      (List.concat_map
         (fun sp ->
           let _, _, f, l = model text sp in
           shown_lines f l)
         spans)
  in
  let rec gaps = function
    | a :: (b :: _ as rest) when b = a + 2 -> (a + 1) :: gaps rest
    | _ :: rest -> gaps rest
    | [] -> []
  in
  (List.sort compare (shown @ gaps shown), gaps shown)

let labelled_lines =
  Prop.test
    ~name:
      "render with labels: shown lines are the union, a one-line gap filled, \
       ... at longer gaps" ~print:print_labelled gen_labelled
    (fun ((text, primary, secondary, _) as c) ->
      let lines, _ = layout text (primary :: secondary) in
      let expected =
        List.concat
          (List.mapi
             (fun i l ->
               (if i > 0 && l > List.nth lines (i - 1) + 1 then [ "..." ]
                else [])
               @ [ string_of_int l ])
             lines)
      in
      let observed =
        List.filter_map
          (fun l ->
            let k = first_nonblank l in
            if l = "..." then Some l
            else if k < String.length l && l.[k] >= '0' && l.[k] <= '9' then
              Some
                (List.hd
                   (String.split_on_char ' '
                      (String.sub l k (String.length l - k))))
            else None)
          (rows_of (render_labelled c))
      in
      observed = expected)

let marker_col l =
  let rec go i =
    if i >= String.length l then None
    else if l.[i] = '^' || l.[i] = '-' then Some (i, l.[i])
    else go (i + 1)
  in
  (* only rows of the form "  | ..." *)
  let k = first_nonblank l in
  if k < String.length l && l.[k] = '|' then go (k + 1) else None

(* Start and end (before the line break) of line [n], numbered from 1. *)
let line_bounds text n =
  let rec go line start =
    let stop =
      match String.index_from_opt text start '\n' with
      | Some e -> e
      | None -> String.length text
    in
    if line = n then (start, stop) else go (line + 1) (stop + 1)
  in
  go 1 0

let labelled_markers =
  Prop.test
    ~name:
      "render with labels: every annotation is marked on each of its drawn \
       lines, multiline ones too; rows are ordered"
    ~print:print_labelled gen_labelled
    (fun ((text, primary, secondary, _) as c) ->
      let out = render_labelled c in
      let all =
        (primary, "P0", '^')
        :: List.mapi
             (fun i sp -> (sp, Printf.sprintf "S%d" (i + 1), '-'))
             secondary
      in
      let lines, filled = layout text (List.map (fun (sp, _, _) -> sp) all) in
      let g = String.length (string_of_int (List.fold_left max 0 lines)) in
      (* The marker rows each annotation should have: on its shown lines and on
         the filled gap lines it spans. *)
      let expected =
        List.concat_map
          (fun (sp, tag, ch) ->
            let start, stop, first, last = model text sp in
            let drawn =
              shown_lines first last
              @ List.filter (fun l -> first <= l && l <= last) filled
            in
            List.map
              (fun l ->
                let ls, le = line_bounds text l in
                let from = if l = first then start - ls else 0 in
                let upto = if l = last then min stop le - ls else le - ls in
                String.make g ' ' ^ " | " ^ String.make from ' '
                ^ String.make (max 1 (upto - from)) ch
                ^ if l = last then " " ^ tag else "")
              drawn)
          all
      in
      let rows = rows_of out in
      let observed = List.filter (fun l -> marker_col l <> None) rows in
      let ordered =
        let rec go = function
          | a :: (b :: _ as rest) -> (
              match (marker_col a, marker_col b) with
              | Some (ca, ma), Some (cb, mb) ->
                  (ca < cb || (ca = cb && not (ma = '-' && mb = '^')))
                  && go rest
              | _ -> go rest)
          | _ -> true
        in
        (* marker rows of a source line are consecutive; a source row resets *)
        let rec groups acc cur = function
          | [] -> List.rev (List.rev cur :: acc)
          | l :: rest ->
              if marker_col l <> None then groups acc (l :: cur) rest
              else groups (List.rev cur :: acc) [] rest
        in
        List.for_all go (groups [] [] rows)
      in
      List.sort compare observed = List.sort compare expected && ordered)

let () =
  Prop.run "source_render"
    [
      roundtrip;
      line_tiling;
      counting;
      render_shape;
      render_colour;
      caret_alignment;
      unicode_cells;
      huge_line;
      labelled_shape;
      labelled_lines;
      labelled_markers;
    ]
