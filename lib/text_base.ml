(* The default instance: parsing a string, one byte at a time, plus text
   primitives that scan the string directly. Strings, Lexeme and Utf8 build on
   this module; [Text] puts them together. *)

module String_input = struct
  type t = string
  type token = char

  let length = String.length
  let get = String.get
  let equal = Char.equal
  let show = String.make 1

  let span s i =
    if i >= String.length s then Span.point (String.length s)
    else Span.make ~start:i ~stop:(i + 1)

  let unexpected s i =
    let description, n = Quote.found s i in
    (description, Span.make ~start:i ~stop:(i + n))

  let describe = Quote.range
end

include Engine.Make (String_input)

let tokenize ?(skip = return ()) tok text =
  match parse (skip *> many (spanned tok <* skip) <* eof) text with
  | Ok l -> Ok (Array.of_list l)
  | Error _ as e -> e

let char = token
let any_char = any

let string lit =
  let n = String.length lit in
  let items = [ Expected.Literal lit ] in
  let rec matches s pos i =
    i >= n
    || String.unsafe_get s (pos + i) = String.unsafe_get lit i
       && matches s pos (i + 1)
  in
  fun st pos ->
    if pos + n <= st.len && matches st.input pos 0 then pass lit (pos + n)
    else fail_at pos items

let scan pred s pos =
  let n = String.length s in
  let rec go i =
    if i < n && pred (String.unsafe_get s i) then go (i + 1) else i
  in
  go pos

let stop_hint label stop =
  match label with
  | None -> None
  | Some l -> Some (expect_error stop [ Expected.Label l ])

let take_while ?label pred st pos =
  let stop = scan pred st.input pos in
  Pass
    {
      value = String.sub st.input pos (stop - pos);
      pos = stop;
      cut = false;
      hint = stop_hint label stop;
    }

let take_while1 name pred =
  let items = [ Expected.Label name ] in
  fun st pos ->
    let stop = scan pred st.input pos in
    if stop = pos then fail_at pos items
    else pass (String.sub st.input pos (stop - pos)) stop

let skip_while ?label pred st pos =
  let stop = scan pred st.input pos in
  Pass { value = (); pos = stop; cut = false; hint = stop_hint label stop }

let skip_line st pos =
  match String.index_from_opt st.input pos '\n' with
  | Some i -> pass () (i + 1)
  | None -> pass () st.len

(* Errors for the text primitives *)

(* A custom failure at [pos] whose diagnostic underlines [start, stop). *)
let custom_fail pos ~start ~stop message =
  Fail
    {
      error =
        {
          pos;
          kind = Custom { code = None; message };
          span = Some (Span.make ~start ~stop);
          context = [];
          labels = [];
          notes = [];
        };
      cut = false;
    }

(* Character classes (ASCII only) *)

let is_digit = function '0' .. '9' -> true | _ -> false
let is_letter = function 'a' .. 'z' | 'A' .. 'Z' -> true | _ -> false
let is_alnum c = is_digit c || is_letter c

let is_hex_digit = function
  | '0' .. '9' | 'a' .. 'f' | 'A' .. 'F' -> true
  | _ -> false

let is_space = function ' ' | '\t' | '\n' | '\r' | '\x0c' -> true | _ -> false
let digit = satisfy "digit" is_digit
let letter = satisfy "letter" is_letter
let hex_digit = satisfy "hexadecimal digit" is_hex_digit

let one_of chars =
  let items =
    List.sort_uniq Expected.compare
      (List.init (String.length chars) (fun i ->
           Expected.Literal (String.make 1 chars.[i])))
  in
  fun st pos ->
    if pos < st.len && String.contains chars (String.unsafe_get st.input pos)
    then pass (String.unsafe_get st.input pos) (pos + 1)
    else fail_at pos items

let none_of chars st pos =
  if
    pos < st.len && not (String.contains chars (String.unsafe_get st.input pos))
  then pass (String.unsafe_get st.input pos) (pos + 1)
  else fail_at pos []

let newline =
  let items = [ Expected.Label "newline" ] in
  fun st pos ->
    if pos < st.len then
      match String.unsafe_get st.input pos with
      | '\n' -> pass () (pos + 1)
      | '\r'
        when pos + 1 < st.len && String.unsafe_get st.input (pos + 1) = '\n' ->
          pass () (pos + 2)
      | _ -> fail_at pos items
    else fail_at pos items

let whitespace = hidden (skip_while is_space)

let string_ci lit =
  let n = String.length lit in
  let items = [ Expected.Literal lit ] in
  let lit = String.lowercase_ascii lit in
  let rec matches s pos i =
    i >= n
    || Char.lowercase_ascii (String.unsafe_get s (pos + i))
       = String.unsafe_get lit i
       && matches s pos (i + 1)
  in
  fun st pos ->
    if pos + n <= st.len && matches st.input pos 0 then
      pass (String.sub st.input pos n) (pos + n)
    else fail_at pos items

let consumed p st pos =
  match p st pos with
  | Pass r ->
      Pass
        {
          value = String.sub st.input pos (r.pos - pos);
          pos = r.pos;
          cut = r.cut;
          hint = r.hint;
        }
  | Fail _ as r -> r

(* Numbers: longest-match tokens. *)

let scan_digits s pos = scan is_digit s pos

(* End of the optional fraction / exponent starting at [pos], if complete. *)
let scan_fraction s pos =
  let n = String.length s in
  if pos + 1 < n && s.[pos] = '.' && is_digit s.[pos + 1] then
    scan_digits s (pos + 1)
  else pos

let scan_exponent s pos =
  let n = String.length s in
  if pos < n && (s.[pos] = 'e' || s.[pos] = 'E') then
    let d =
      if pos + 1 < n && (s.[pos + 1] = '+' || s.[pos + 1] = '-') then pos + 2
      else pos + 1
    in
    if d < n && is_digit s.[d] then scan_digits s d else pos
  else pos

(* [number_end s pos]: the end of the number token at [pos], or [pos] if there
   is none. [~int]: no fraction or exponent; [~neg]: allow a leading '-'. *)
let number_end ~int ~neg s pos =
  let n = String.length s in
  let d = if neg && pos < n && s.[pos] = '-' then pos + 1 else pos in
  let e = scan_digits s d in
  if e = d then pos else if int then e else scan_exponent s (scan_fraction s e)

let number_token name ~int ~neg convert message =
  let items = [ Expected.Label name ] in
  fun st pos ->
    let stop = number_end ~int ~neg st.input pos in
    if stop = pos then fail_at pos items
    else
      match convert (String.sub st.input pos (stop - pos)) with
      | Some v -> pass v stop
      | None -> custom_fail stop ~start:pos ~stop message

let natural =
  number_token "integer" ~int:true ~neg:false int_of_string_opt
    "integer out of range"

let integer =
  number_token "integer" ~int:true ~neg:true int_of_string_opt
    "integer out of range"

let number =
  number_token "number" ~int:false ~neg:true
    (fun s ->
      let f = float_of_string s in
      if Float.is_finite f then Some f else None)
    "number out of range"
