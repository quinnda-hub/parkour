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
      (list_size (int_bound 30) (oneof [ frag fragments; frag newline_heavy ])))

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
  Prop.test ~name:"render: caret starts under the span's column (ASCII line)"
    ~print:Print.(triple print_text int int)
    gen_ascii_line
    (fun (s, a, l) ->
      let d = Diagnostic.make ~span:(Span.make ~start:a ~stop:(a + l)) "m" in
      let out = Diagnostic.render (Source.make s) d in
      match String.split_on_char '\n' out with
      | _ :: _ :: _ :: _ :: caret :: _ ->
          String.index caret '^' = 4 + a
          && String.length (String.concat "" (String.split_on_char '^' caret))
             = String.length caret - max 1 l
      | _ -> false)

let huge_line =
  Prop.test ~count:1 ~name:"render: one error in a 10 MB single line is cheap"
    (Gen.return ()) (fun () ->
      let n = 10_000_000 in
      let s = String.make n 'a' in
      let out =
        Diagnostic.render (Source.make s)
          (Diagnostic.make ~span:(Span.make ~start:(n - 5) ~stop:(n - 1)) "m")
      in
      String.length out < 1000)

let () =
  Prop.run "source_render"
    [
      roundtrip;
      line_tiling;
      counting;
      render_shape;
      render_colour;
      caret_alignment;
      huge_line;
    ]
