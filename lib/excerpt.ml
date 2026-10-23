(* The layout of source lines for [Diagnostic.render]: display columns, tab
   expansion, control-character substitution and horizontal windowing. *)

type seg =
  | Plain of string
  | Gutter of string
  | Sev of string
  | Sec of string
  | Msg of string

let seg_text = function Plain s | Gutter s | Sev s | Sec s | Msg s -> s

let map_seg f = function
  | Plain s -> Plain (f s)
  | Gutter s -> Gutter (f s)
  | Sev s -> Sev (f s)
  | Sec s -> Sec (f s)
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

(* Display columns of [p0, p1) within the line [ls, le): the line's total
   width and the part's start and end columns. *)
let measure text ~tab_width ls le p0 p1 =
  let total = ref 0 and c0 = ref 0 and c1 = ref 0 in
  iter_cells text ~tab_width ls le (fun byte len _ col width ->
      total := col + width;
      if byte + len <= p0 then c0 := !total;
      if byte + len <= p1 then c1 := !total);
  (!total, !c0, max !c0 !c1)

type line_view = {
  content : string;  (** displayed text, including any … *)
  marks : (int * int) list;
      (** for each requested part: marker column (relative to the shown text)
          and marker count *)
}

(* Shows the line [ls, le) with each byte range of [parts] marked. The window,
   if the line is too wide, follows [anchor]; a part outside the window is
   drawn as one marker at the nearest visible column. *)
let view_line text ~tab_width ~max_width ls le ~anchor:(q0, q1) parts =
  let total, c0, c1 = measure text ~tab_width ls le q0 q1 in
  let cend = c0 + max 1 (c1 - c0) in
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
  let mark (p0, p1) =
    let _, c0, c1 = measure text ~tab_width ls le p0 p1 in
    let cend = c0 + max 1 (c1 - c0) in
    let s = min (max c0 a) (b - 1) in
    (s - w0, max (s + 1) (min cend b) - s)
  in
  { content = Buffer.contents buf; marks = List.map mark parts }
