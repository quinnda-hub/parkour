type severity = [ `Error | `Warning ]

type t = {
  severity : severity;
  code : string option;
  span : Span.t;
  message : string;
  expected : Expected.t list;
  found : string option;
  context : (string * Span.t) list;
  label : string option;
  labels : (Span.t * string) list;
  notes : string list;
}

let severity t = t.severity
let code t = t.code
let span t = t.span
let message t = t.message
let expected t = t.expected
let found t = t.found
let context t = t.context
let label t = t.label
let labels t = t.labels
let notes t = t.notes

let make ?(severity = `Error) ?code ?(expected = []) ?found ?(context = [])
    ?label ?(labels = []) ?(notes = []) ~span message =
  let expected = List.sort_uniq Expected.compare expected in
  {
    severity;
    code;
    span;
    message;
    expected;
    found;
    context;
    label;
    labels;
    notes;
  }

let add_label span text t = { t with labels = t.labels @ [ (span, text) ] }
let add_note note t = { t with notes = t.notes @ [ note ] }
let is_error t = t.severity = `Error

type location = [ `Line_column | `Char_offset | `Byte_offset ]

let severity_name = function `Error -> "error" | `Warning -> "warning"

let headline t =
  severity_name t.severity
  ^ match t.code with None -> "" | Some c -> "[" ^ c ^ "]"

let clamp_offset src o = max 0 (min o (String.length (Source.text src)))

(* Position of [offset] in the chosen style, without the file name. *)
let where src style offset =
  let offset = clamp_offset src offset in
  match style with
  | `Line_column ->
      let p = Source.position src offset in
      Printf.sprintf "%d:%d" p.line p.column
  | `Char_offset -> Printf.sprintf "char %d" (Source.char_offset src offset)
  | `Byte_offset -> Printf.sprintf "byte %d" offset

let where_named src style offset =
  let w = where src style offset in
  match (Source.name src, style) with
  | None, _ -> w
  | Some n, `Line_column -> n ^ ":" ^ w
  | Some n, _ -> n ^ ", " ^ w

let pp ppf t =
  let loc =
    if Span.is_empty t.span then Printf.sprintf "byte %d" t.span.start
    else Printf.sprintf "bytes %d-%d" t.span.start t.span.stop
  in
  Format.fprintf ppf "%s: %s: %s" loc (headline t) t.message

let pp_located ?(location = `Line_column) source ppf t =
  Format.fprintf ppf "%s: %s: %s"
    (where_named source location t.span.start)
    (headline t) t.message

(* Rendering *)

open Excerpt

(* One underlined region: the primary span or a secondary label. *)
type annotation = {
  primary : bool;
  start : int;
  stop : int;  (** trimmed of trailing whitespace *)
  text : string;
  first : int;  (** first and last line *)
  last : int;
  shown : int list;  (** the lines drawn: all, or first two and last *)
}

let render ?(location = `Line_column) ?(color = false) ?(max_context = 2)
    ?(tab_width = 4) ?(max_width = 100) src t =
  let text = Source.text src in
  let tab_width = max 1 tab_width and max_width = max 10 max_width in
  let line_of o = (Source.position src o).line in
  let annotation ~primary ~text:caption (sp : Span.t) =
    let start = clamp_offset src sp.start in
    let stop = max start (clamp_offset src sp.stop) in
    (* Markers never cover trailing whitespace (e.g. what a lexeme skipped),
       unless the span is all whitespace. *)
    let stop =
      let rec trim e =
        if e > start then
          match text.[e - 1] with
          | ' ' | '\t' | '\n' | '\r' | '\x0c' -> trim (e - 1)
          | _ -> e
        else stop
      in
      trim stop
    in
    let first = line_of start in
    let last = if stop > start then line_of (stop - 1) else first in
    let shown =
      if last - first + 1 > 4 then [ first; first + 1; last ]
      else List.init (last - first + 1) (fun k -> first + k)
    in
    { primary; start; stop; text = caption; first; last; shown }
  in
  let primary_text =
    match (t.label, t.expected) with
    | Some l, _ -> l
    | None, [] -> ""
    | None, l -> "expected " ^ Expected.list_to_string l
  in
  let primary = annotation ~primary:true ~text:primary_text t.span in
  let annotations =
    primary
    :: List.map (fun (sp, text) -> annotation ~primary:false ~text sp) t.labels
  in
  let lines =
    List.sort_uniq compare (List.concat_map (fun a -> a.shown) annotations)
  in
  (* A gap of a single line is shown rather than replaced by "...". *)
  let filled =
    let rec go = function
      | a :: (b :: _ as rest) when b = a + 2 -> (a + 1) :: go rest
      | _ :: rest -> go rest
      | [] -> []
    in
    go lines
  in
  let lines = List.sort compare (lines @ filled) in
  let shows a line =
    List.mem line a.shown
    || (List.mem line filled && a.first <= line && line <= a.last)
  in
  let g = digits (List.fold_left max 0 lines) in
  let sev_code =
    match t.severity with `Error -> "\027[1;31m" | `Warning -> "\027[1;33m"
  in
  let out = Buffer.create 256 in
  let emit segs =
    List.iter
      (fun seg ->
        let code =
          match seg with
          | Plain _ -> ""
          | Gutter _ | Sec _ -> "\027[1;34m"
          | Sev _ -> sev_code
          | Msg _ -> "\027[1m"
        in
        if color && code <> "" then (
          Buffer.add_string out code;
          Buffer.add_string out (seg_text seg);
          Buffer.add_string out "\027[0m")
        else Buffer.add_string out (seg_text seg))
      (trim_segs segs);
    Buffer.add_char out '\n'
  in
  emit [ Sev (headline t); Plain ": "; Msg t.message ];
  emit
    [
      Plain (spaces g);
      Gutter "-->";
      Plain (" " ^ where_named src location primary.start);
    ];
  let bar = [ Plain (spaces g ^ " "); Gutter "|" ] in
  emit bar;
  let draw_line line =
    let sp = Source.line_span src line in
    let ls = sp.start and le = sp.stop in
    let touching = List.filter (fun a -> shows a line) annotations in
    let part a =
      let p0 = if line = a.first then min a.start le else ls in
      let p1 = if line = a.last then min a.stop le else le in
      (p0, max p0 p1)
    in
    let parts = List.map part touching in
    let anchor =
      match List.find_opt (fun a -> a.primary) touching with
      | Some a -> part a
      | None -> (
          match parts with
          | [] -> (ls, ls)
          | p :: rest ->
              List.fold_left
                (fun ((m0, _) as m) ((p0, _) as p) -> if p0 < m0 then p else m)
                p rest)
    in
    let v = view_line text ~tab_width ~max_width ls le ~anchor parts in
    let num = string_of_int line in
    emit
      [
        Plain (spaces (g - String.length num));
        Gutter num;
        Plain " ";
        Gutter "|";
        Plain (" " ^ v.content);
      ];
    let rows = List.map2 (fun a (col, n) -> (a, col, n)) touching v.marks in
    let key (a, col, _) = (col, if a.primary then 0 else 1) in
    List.iter
      (fun (a, col, n) ->
        let markers = String.make n (if a.primary then '^' else '-') in
        let markers =
          if line = a.last && a.text <> "" then markers ^ " " ^ a.text
          else markers
        in
        emit
          (bar
          @ [
              Plain (" " ^ spaces col);
              (if a.primary then Sev markers else Sec markers);
            ]))
      (List.stable_sort (fun x y -> compare (key x) (key y)) rows)
  in
  ignore
    (List.fold_left
       (fun prev line ->
         if prev > 0 && line > prev + 1 then emit [ Gutter "..." ];
         draw_line line;
         line)
       0 lines);
  let ctx = t.context in
  let frame (name, (sp : Span.t)) =
    emit
      [
        Plain (spaces g ^ " ");
        Gutter "=";
        Plain
          (" while parsing " ^ name ^ ", from " ^ where src location sp.start);
      ]
  in
  let len = List.length ctx in
  let m = max 0 max_context in
  if len <= m then List.iter frame ctx
  else begin
    List.iteri (fun i f -> if i < m then frame f) ctx;
    let omitted = len - m - 1 in
    if omitted > 0 then
      emit
        [
          Plain (spaces g ^ " ");
          Gutter "=";
          Plain (Printf.sprintf " ... and %d more" omitted);
        ];
    frame (List.nth ctx (len - 1))
  end;
  List.iter
    (fun note ->
      emit
        [
          Plain (spaces g ^ " ");
          Gutter "=";
          Plain " ";
          Msg "note:";
          Plain (" " ^ note);
        ])
    t.notes;
  Buffer.contents out

let pp_rendered ?location ?color ?max_context ?tab_width ?max_width src ppf t =
  Format.pp_print_string ppf
    (render ?location ?color ?max_context ?tab_width ?max_width src t)
