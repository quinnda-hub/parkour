(* The default instance: parsing a string, one byte at a time, plus text
   primitives that scan the string directly. *)

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
