type severity = [ `Error | `Warning ]

type t = {
  severity : severity;
  code : string option;
  span : Span.t;
  message : string;
  expected : Expected.t list;
  found : string option;
  context : (string * Span.t) list;
}

let severity t = t.severity
let code t = t.code
let span t = t.span
let message t = t.message
let expected t = t.expected
let found t = t.found
let context t = t.context

let make ?(severity = `Error) ?code ?(expected = []) ?found ?(context = [])
    ~span message =
  let expected = List.sort_uniq Expected.compare expected in
  { severity; code; span; message; expected; found; context }

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

type seg = Plain of string | Gutter of string | Sev of string | Msg of string

let seg_text = function Plain s | Gutter s | Sev s | Msg s -> s

let map_seg f = function
  | Plain s -> Plain (f s)
  | Gutter s -> Gutter (f s)
  | Sev s -> Sev (f s)
  | Msg s -> Msg (f s)

let rstrip s =
  let n = ref (String.length s) in
  while !n > 0 && s.[!n - 1] = ' ' do
    decr n
  done;
  String.sub s 0 !n

(* Drop trailing spaces across segments. *)
let trim_segs segs =
  let rec go = function
    | [] -> []
    | seg :: rest ->
        let seg = map_seg rstrip seg in
        if seg_text seg = "" then go rest else seg :: rest
  in
  List.rev (go (List.rev segs))

let spaces n = String.make (max 0 n) ' '
let digits n = String.length (string_of_int n)

let utf8 code =
  let b = Buffer.create 4 in
  Buffer.add_utf_8_uchar b (Uchar.of_int code);
  Buffer.contents b

(* Characters shown as U+FFFD: C1 controls and bidirectional controls. *)
let is_odd c =
  (0x80 <= c && c <= 0x9f)
  || (0x202A <= c && c <= 0x202E)
  || (0x2066 <= c && c <= 0x2069)

(* Calls [f byte len code col width] for each character of text[start, stop),
   where [col] is its display column relative to [start]. *)
let iter_cells text ~tab_width start stop f =
  let i = ref start and col = ref 0 in
  while !i < stop do
    let d = Decode.decode text !i in
    let len = min (Decode.length d) (stop - !i) in
    let code = Decode.code d in
    let width =
      if code = 9 then tab_width - (!col mod tab_width)
      else if code < 0 || code < 0x20 || code = 0x7f || is_odd code then 1
      else Width.of_code code
    in
    f !i len code !col width;
    col := !col + width;
    i := !i + len
  done

let cell_text text byte len code width =
  if code = 9 then spaces width
  else if code < 0 || is_odd code then "\xef\xbf\xbd"
  else if code < 0x20 then utf8 (0x2400 + code)
  else if code = 0x7f then utf8 0x2421
  else String.sub text byte len

type line_view = {
  content : string; (* displayed text, including any … *)
  caret_col : int; (* relative to the shown text *)
  carets : int;
}

(* [p0, p1) is the span's part of the line [ls, le). *)
let view_line text ~tab_width ~max_width ls le p0 p1 =
  let total = ref 0 and c0 = ref 0 and c1 = ref 0 in
  iter_cells text ~tab_width ls le (fun byte len _ col width ->
      total := col + width;
      if byte + len <= p0 then c0 := !total;
      if byte + len <= p1 then c1 := !total);
  let total = !total and c0 = !c0 and c1 = max !c0 !c1 in
  let carets = max 1 (c1 - c0) in
  let cend = c0 + carets in
  let w0, w1, left, right =
    if total <= max_width then (0, max_int, false, false)
    else
      let total' = max total cend in
      let w0 = max 0 (c0 - (max_width / 3)) in
      let w0 = min w0 (max 0 (total' - max_width)) in
      let w1 = w0 + max_width in
      (w0, w1, w0 > 0, w1 < total)
  in
  let a = if left then w0 + 1 else 0
  and b = if right then w1 - 1 else max_int in
  let buf = Buffer.create 128 in
  if left then Buffer.add_string buf "\xe2\x80\xa6";
  iter_cells text ~tab_width ls le (fun byte len code col width ->
      if width = 0 then
        begin if a <= col && col < b then
          Buffer.add_string buf (cell_text text byte len code width)
        end
      else
        let lo = max a col and hi = min b (col + width) in
        if hi - lo = width then
          Buffer.add_string buf (cell_text text byte len code width)
        else if hi > lo then Buffer.add_string buf (spaces (hi - lo)));
  if right then Buffer.add_string buf "\xe2\x80\xa6";
  let s, e =
    if w1 = max_int then (c0, cend)
    else
      let s = min (max c0 a) (b - 1) in
      (s, max (s + 1) (min cend b))
  in
  { content = Buffer.contents buf; caret_col = s - w0; carets = e - s }

let render ?(location = `Line_column) ?(color = false) ?(max_context = 2)
    ?(tab_width = 4) ?(max_width = 100) src t =
  let text = Source.text src in
  let tab_width = max 1 tab_width and max_width = max 10 max_width in
  let start = clamp_offset src t.span.start in
  let stop = max start (clamp_offset src t.span.stop) in
  (* Carets never cover trailing whitespace (e.g. what a lexeme skipped),
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
  let line_of o = (Source.position src o).line in
  let l1 = line_of start in
  let l2 = if stop > start then line_of (stop - 1) else l1 in
  let shown =
    if l2 - l1 + 1 > 4 then [ Some l1; Some (l1 + 1); None; Some l2 ]
    else List.init (l2 - l1 + 1) (fun k -> Some (l1 + k))
  in
  let g = digits l2 in
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
          | Gutter _ -> "\027[1;34m"
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
      Plain (" " ^ where_named src location start);
    ];
  let bar = [ Plain (spaces g ^ " "); Gutter "|" ] in
  emit bar;
  let label =
    match t.expected with
    | [] -> ""
    | l -> " expected " ^ Expected.list_to_string l
  in
  List.iter
    (function
      | None -> emit [ Gutter "..." ]
      | Some line ->
          let sp = Source.line_span src line in
          let ls = sp.start and le = sp.stop in
          let p0 = if line = l1 then min start le else ls in
          let p1 = if line = l2 then min stop le else le in
          let p1 = max p0 p1 in
          let v = view_line text ~tab_width ~max_width ls le p0 p1 in
          let num = string_of_int line in
          emit
            [
              Plain (spaces (g - String.length num));
              Gutter num;
              Plain " ";
              Gutter "|";
              Plain (" " ^ v.content);
            ];
          emit
            (bar
            @ [
                Plain (" " ^ spaces v.caret_col);
                Sev (String.make v.carets '^' ^ if line = l2 then label else "");
              ]))
    shown;
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
  Buffer.contents out

let pp_rendered ?location ?color ?max_context ?tab_width ?max_width src ppf t =
  Format.pp_print_string ppf
    (render ?location ?color ?max_context ?tab_width ?max_width src t)
