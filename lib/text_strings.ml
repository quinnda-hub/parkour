(* Quoted strings and the JSON escape sequences. *)

open Text_base

let no_progress pos =
  raise
    (Parkour_intf.Grammar_error
       (Printf.sprintf
          "Parkour.quoted: the escape parser succeeded without consuming input \
           at position %d, so it would repeat forever"
          pos))

(* [quoted] is a loop over the raw text; only escapes call a parser. [chunk]
   is the start of the raw text not yet copied to [buf]. *)
let quoted ?escape ?(doubled = false) ?(multiline = false) quote =
  let items = [ Expected.Label "string" ] in
  let backslash = escape <> None in
  fun st start ->
    let s = st.input in
    let unterminated i =
      custom_fail i ~start
        ~stop:(if multiline then start + 1 else i)
        "unterminated string"
    in
    let rec go buf chunk i =
      if i >= st.len then unterminated i
      else
        let c = String.unsafe_get s i in
        if c = quote && doubled && i + 1 < st.len && s.[i + 1] = quote then begin
          (* Keep the first quote of the pair, drop the second. *)
          let b = match buf with Some b -> b | None -> Buffer.create 64 in
          Buffer.add_substring b s chunk (i + 1 - chunk);
          go (Some b) (i + 2) (i + 2)
        end
        else if c = quote then begin
          let tail = String.sub s chunk (i - chunk) in
          match buf with
          | None -> pass tail (i + 1)
          | Some b ->
              Buffer.add_string b tail;
              pass (Buffer.contents b) (i + 1)
        end
        else if (c = '\n' || c = '\r') && not multiline then unterminated i
        else if c = '\\' && backslash then
          begin if i + 1 >= st.len then unterminated (i + 1)
          else
            let log = st.log and errors = st.errors in
            match (Option.get escape) st i with
            | Pass r ->
                if r.pos <= i then no_progress i;
                let b =
                  match buf with Some b -> b | None -> Buffer.create 64
                in
                Buffer.add_substring b s chunk (i - chunk);
                Buffer.add_string b r.value;
                go (Some b) r.pos r.pos
            | Fail { error = { kind = Expect _; pos; _ }; cut = false }
              when pos = i || pos = i + 1 ->
                restore st log errors;
                let n = Decode.length (Decode.decode s (i + 1)) in
                custom_fail i ~start:i
                  ~stop:(i + 1 + n)
                  "invalid escape sequence"
            | Fail r -> Fail { r with cut = false }
          end
        else go buf chunk (i + 1)
    in
    if start < st.len && String.unsafe_get s start = quote then
      go None (start + 1) (start + 1)
    else fail_at start items

let hex_value = function
  | '0' .. '9' as c -> Char.code c - 48
  | 'a' .. 'f' as c -> Char.code c - 87
  | c -> Char.code c - 55

(* \uXXXX at [i] (the backslash): the end of its hex digits, up to four, and
   the code unit if all four are present. *)
let hex4 s len i =
  let rec go j v =
    if j >= len || j - (i + 2) = 4 || not (is_hex_digit (String.unsafe_get s j))
    then (j, if j - (i + 2) = 4 then Some v else None)
    else go (j + 1) ((v * 16) + hex_value (String.unsafe_get s j))
  in
  go (i + 2) 0

let has_u s len i = i + 1 < len && s.[i] = '\\' && s.[i + 1] = 'u'

let json_escape =
  let bad_unicode i stop =
    custom_fail i ~start:i ~stop "invalid unicode escape"
  in
  let items = [ Expected.Label "escape sequence" ] in
  fun st i ->
    let s = st.input and len = st.len in
    if i >= len || s.[i] <> '\\' then fail_at i [ Expected.Literal "\\" ]
    else if i + 1 >= len then fail_at (i + 1) items
    else
      let simple c = pass (String.make 1 c) (i + 2) in
      match s.[i + 1] with
      | ('"' | '\\' | '/') as c -> simple c
      | 'b' -> simple '\b'
      | 'f' -> simple '\x0c'
      | 'n' -> simple '\n'
      | 'r' -> simple '\r'
      | 't' -> simple '\t'
      | 'u' -> (
          let stop, code = hex4 s len i in
          let encode u =
            let b = Buffer.create 4 in
            Buffer.add_utf_8_uchar b (Uchar.of_int u);
            Buffer.contents b
          in
          match code with
          | None -> bad_unicode i stop
          | Some u when u < 0xD800 || u > 0xDFFF -> pass (encode u) stop
          | Some u when u >= 0xDC00 -> bad_unicode i stop
          | Some hi -> (
              if not (has_u s len stop) then bad_unicode i stop
              else
                let stop2, code2 = hex4 s len stop in
                match code2 with
                | Some lo when lo >= 0xDC00 && lo <= 0xDFFF ->
                    pass
                      (encode
                         (0x10000 + ((hi - 0xD800) lsl 10) + (lo - 0xDC00)))
                      stop2
                | Some _ -> bad_unicode i stop2
                | None -> bad_unicode i stop))
      | _ -> fail_at (i + 1) items

let string_literal = quoted ~escape:json_escape '"'
