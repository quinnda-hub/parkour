(* Scalar-level primitives. An invalid byte is an error with a custom message
   rather than a mismatch, so that it is never mistaken for an expectation. *)

open Text_base

let invalid_message = "invalid UTF-8"

let invalid_error pos =
  {
    pos;
    kind = Custom { code = None; message = invalid_message };
    span = Some (Span.make ~start:pos ~stop:(pos + 1));
    context = [];
  }

let invalid_fail pos = Fail { error = invalid_error pos; cut = false }
let uchar d = Uchar.unsafe_of_int (Decode.code d)
let is_invalid d = Decode.code d < 0

let any st pos =
  if pos >= st.len then fail_at pos []
  else
    let d = Decode.decode st.input pos in
    if is_invalid d then invalid_fail pos
    else pass (uchar d) (pos + Decode.length d)

let char u =
  let enc =
    let b = Buffer.create 4 in
    Buffer.add_utf_8_uchar b u;
    Buffer.contents b
  in
  let items = [ Expected.Literal enc ] in
  fun st pos ->
    if pos >= st.len then fail_at pos items
    else
      let d = Decode.decode st.input pos in
      if is_invalid d then invalid_fail pos
      else if Decode.code d = Uchar.to_int u then pass u (pos + Decode.length d)
      else fail_at pos items

let satisfy name pred =
  let items = [ Expected.Label name ] in
  fun st pos ->
    if pos >= st.len then fail_at pos items
    else
      let d = Decode.decode st.input pos in
      if is_invalid d then invalid_fail pos
      else if pred (uchar d) then pass (uchar d) (pos + Decode.length d)
      else fail_at pos items

let scan pred s len pos =
  let rec go i =
    if i >= len then i
    else
      let d = Decode.decode s i in
      if (not (is_invalid d)) && pred (uchar d) then go (i + Decode.length d)
      else i
  in
  go pos

(* What a scan that stopped at [stop] leaves for the next parser. *)
let stop_hint label st stop =
  if stop < st.len && is_invalid (Decode.decode st.input stop) then
    Some (invalid_error stop)
  else stop_hint label stop

let take_while ?label pred st pos =
  let stop = scan pred st.input st.len pos in
  Pass
    {
      value = String.sub st.input pos (stop - pos);
      pos = stop;
      cut = false;
      hint = stop_hint label st stop;
    }

let skip_while ?label pred st pos =
  let stop = scan pred st.input st.len pos in
  Pass { value = (); pos = stop; cut = false; hint = stop_hint label st stop }

let take_while1 name pred =
  let items = [ Expected.Label name ] in
  fun st pos ->
    let stop = scan pred st.input st.len pos in
    if stop > pos then
      Pass
        {
          value = String.sub st.input pos (stop - pos);
          pos = stop;
          cut = false;
          hint = stop_hint None st stop;
        }
    else if stop < st.len && is_invalid (Decode.decode st.input stop) then
      invalid_fail stop
    else fail_at pos items
