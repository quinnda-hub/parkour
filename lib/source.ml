type position = { offset : int; line : int; column : int }

type t = {
  name : string option;
  text : string;
  mutable starts : int array option; (* offset of the first byte of each line *)
}

let make ?name text = { name; text; starts = None }
let name t = t.name
let text t = t.text

let line_starts t =
  match t.starts with
  | Some a -> a
  | None ->
      let n = String.length t.text in
      let count = ref 1 in
      String.iter (fun c -> if c = '\n' then incr count) t.text;
      let a = Array.make !count 0 in
      let k = ref 1 in
      for i = 0 to n - 1 do
        if String.unsafe_get t.text i = '\n' then begin
          a.(!k) <- i + 1;
          incr k
        end
      done;
      t.starts <- Some a;
      a

let line_count t = Array.length (line_starts t)

(* 0-based index of the line containing [offset] *)
let line_index starts offset =
  let lo = ref 0 and hi = ref (Array.length starts - 1) in
  while !lo < !hi do
    let mid = (!lo + !hi + 1) / 2 in
    if starts.(mid) <= offset then lo := mid else hi := mid - 1
  done;
  !lo

let line_stop t starts k =
  let n = String.length t.text in
  if k + 1 < Array.length starts then
    let e = starts.(k + 1) - 1 in
    if e > starts.(k) && t.text.[e - 1] = '\r' then e - 1 else e
  else n

let line_span t line =
  let starts = line_starts t in
  if line < 1 || line > Array.length starts then
    invalid_arg "Source.line_span: line out of range";
  Span.make ~start:starts.(line - 1) ~stop:(line_stop t starts (line - 1))

let check t offset =
  if offset < 0 || offset > String.length t.text then
    invalid_arg "Source: offset out of range"

(* Number of characters lying entirely in [from, upto), and their UTF-16
   length. *)
let count_chars text from upto =
  let chars = ref 0 and utf16 = ref 0 and i = ref from in
  while !i < upto do
    let d = Width.decode text !i in
    let l = Width.length d in
    if !i + l <= upto then begin
      incr chars;
      utf16 := !utf16 + if Width.code d >= 0x10000 then 2 else 1
    end;
    i := !i + l
  done;
  (!chars, !utf16)

let position t offset =
  check t offset;
  let starts = line_starts t in
  let k = line_index starts offset in
  let chars, _ =
    count_chars t.text starts.(k) (min offset (line_stop t starts k))
  in
  { offset; line = k + 1; column = chars + 1 }

let char_offset t offset =
  check t offset;
  fst (count_chars t.text 0 offset)

let utf16_column t offset =
  check t offset;
  let starts = line_starts t in
  let k = line_index starts offset in
  snd (count_chars t.text starts.(k) (min offset (line_stop t starts k)))

let offset t ~line ~column =
  let starts = line_starts t in
  if line < 1 || line > Array.length starts then
    invalid_arg "Source.offset: line out of range";
  if column < 1 then invalid_arg "Source.offset: column must be >= 1";
  let stop = line_stop t starts (line - 1) in
  let i = ref starts.(line - 1) and c = ref (column - 1) in
  while !c > 0 && !i < stop do
    i := !i + min (Width.length (Width.decode t.text !i)) (stop - !i);
    decr c
  done;
  !i
