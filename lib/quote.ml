(* Length of the well-formed multi-byte scalar at [i], or 0. *)
let multibyte_len s i =
  let d = Decode.decode s i in
  if Decode.code d >= 0 && Decode.length d >= 2 then Decode.length d else 0

let is_ascii_word c =
  (c >= 'a' && c <= 'z')
  || (c >= 'A' && c <= 'Z')
  || (c >= '0' && c <= '9')
  || c = '_'

(* Length of the word character at [i], or 0 if it is not one. *)
let word_len s i = if is_ascii_word s.[i] then 1 else multibyte_len s i

(* Bidirectional controls, which could reorder the displayed message. *)
let is_bidi c =
  c = 0x061C || c = 0x200E || c = 0x200F
  || (0x202A <= c && c <= 0x202E)
  || (0x2066 <= c && c <= 0x2069)

let literal s =
  let n = String.length s in
  let single = n = 1 || (n > 1 && multibyte_len s 0 = n) in
  let q = if single && s <> "'" then '\'' else '"' in
  let b = Buffer.create (n + 2) in
  Buffer.add_char b q;
  let i = ref 0 in
  while !i < n do
    let c = s.[!i] in
    let m = multibyte_len s !i in
    if m > 0 then begin
      let code = Decode.code (Decode.decode s !i) in
      if is_bidi code then Buffer.add_string b (Printf.sprintf "\\u{%X}" code)
      else Buffer.add_substring b s !i m;
      i := !i + m
    end
    else begin
      (match c with
      | '\n' -> Buffer.add_string b "\\n"
      | '\t' -> Buffer.add_string b "\\t"
      | '\r' -> Buffer.add_string b "\\r"
      | '\\' -> Buffer.add_string b "\\\\"
      | c when c = q ->
          Buffer.add_char b '\\';
          Buffer.add_char b c
      | c when c < ' ' || c >= '\x7f' ->
          Buffer.add_string b (Printf.sprintf "\\x%02x" (Char.code c))
      | c -> Buffer.add_char b c);
      incr i
    end
  done;
  Buffer.add_char b q;
  Buffer.contents b

let max_run = 32

let found s i =
  let n = String.length s in
  let lit len = (literal (String.sub s i len), len) in
  match s.[i] with
  | '\n' -> ("newline", 1)
  | '\r' ->
      if i + 1 < n && s.[i + 1] = '\n' then ("newline", 2)
      else ("carriage return", 1)
  | '\t' -> ("tab", 1)
  | ' ' -> ("space", 1)
  | _ ->
      if word_len s i > 0 then begin
        let j = ref i in
        let continue = ref true in
        while !continue && !j < n do
          let w = word_len s !j in
          if w > 0 && !j + w - i <= max_run then j := !j + w
          else continue := false
        done;
        let len = !j - i in
        let d, _ = lit len in
        ((if !j < n && word_len s !j > 0 then d ^ "\u{2026}" else d), len)
      end
      else
        let m = multibyte_len s i in
        lit (if m > 0 then m else 1)

let range s start stop =
  let d, n = found s start in
  if start + n = stop then d
  else
    (* Quote the range itself, truncated at [max_run] bytes on a scalar
       boundary. *)
    let rec fit i =
      if i >= stop then i
      else
        let m = multibyte_len s i in
        let m = if m = 0 || i + m > stop then 1 else m in
        if i + m - start > max_run then i else fit (i + m)
    in
    let e = max (fit start) (min stop (start + 1)) in
    literal (String.sub s start (e - start))
    ^ if e < stop then "\u{2026}" else ""
