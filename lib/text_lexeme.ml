(* Whitespace and comment policies, and the tokens built on them. *)

open Text_base

module type S = sig
  val space : unit t
  val lexeme : 'a t -> 'a t
  val symbol : string -> string t
  val punct : string -> unit t
  val keyword : string -> string t
  val kw : string -> unit t
  val identifier : string t
end

let starts_with s len i lit =
  let n = String.length lit in
  let rec go k =
    k >= n
    || (String.unsafe_get s (i + k) = String.unsafe_get lit k && go (k + 1))
  in
  i + n <= len && go 0

let invalid fmt =
  Printf.ksprintf (fun m -> invalid_arg ("Parkour.Lexeme.make: " ^ m)) fmt

let make ?(line_comments = []) ?(block_comments = []) ?(nested_comments = false)
    ?(newlines = true) ?(ident_start = fun c -> is_letter c || c = '_')
    ?(ident_char = fun c -> is_alnum c || c = '_') ?(reserved = []) () =
  if List.mem "" line_comments then invalid "empty line comment delimiter";
  List.iter
    (fun (o, c) ->
      if o = "" || c = "" then invalid "empty block comment delimiter";
      if nested_comments && o = c then
        invalid "nested comments need distinct delimiters (%S)" o)
    block_comments;
  let reserved_words = Hashtbl.create 16 in
  List.iter (fun w -> Hashtbl.replace reserved_words w ()) reserved;
  let blank c = is_space c && (newlines || not (c = '\n' || c = '\r')) in
  (* The index just past the block comment opened at [start] with [o] (already
     matched) and closed by [c], or -1 if it is unterminated. *)
  let block_end s len start (o, c) =
    let lo = String.length o and lc = String.length c in
    let rec go i depth =
      if i >= len then -1
      else if starts_with s len i c then
        if depth = 1 then i + lc else go (i + lc) (depth - 1)
      else if nested_comments && starts_with s len i o then
        go (i + lo) (depth + 1)
      else go (i + 1) depth
    in
    go (start + lo) 1
  in
  let rec line_end s len i =
    if i < len && s.[i] <> '\n' && s.[i] <> '\r' then line_end s len (i + 1)
    else i
  in
  let space st pos =
    let s = st.input and len = st.len in
    let finish i =
      if i > pos then note_trivia st pos i;
      pass () i
    in
    let rec go i =
      if i >= len then finish i
      else if blank (String.unsafe_get s i) then go (i + 1)
      else
        (* Block openers first: a line opener may prefix one ("--", "--[["). *)
        match
          List.find_opt (fun (o, _) -> starts_with s len i o) block_comments
        with
        | Some ((o, _) as delims) ->
            let e = block_end s len i delims in
            if e < 0 then
              custom_fail len ~start:i
                ~stop:(i + String.length o)
                "unterminated comment"
            else go e
        | None ->
            if List.exists (starts_with s len i) line_comments then
              go (line_end s len i)
            else finish i
    in
    go pos
  in
  let lexeme p = p <* space in
  let symbol lit = lexeme (string lit) in
  let punct lit = map ignore (symbol lit) in
  let keyword word =
    let n = String.length word in
    let items = [ Expected.Literal word ] in
    lexeme (fun st pos ->
        if
          starts_with st.input st.len pos word
          && not
               (pos + n < st.len
               && ident_char (String.unsafe_get st.input (pos + n)))
        then pass word (pos + n)
        else fail_at pos items)
  in
  let kw word = map ignore (keyword word) in
  let identifier =
    let items = [ Expected.Label "identifier" ] in
    lexeme (fun st pos ->
        if pos < st.len && ident_start (String.unsafe_get st.input pos) then
          let stop = scan ident_char st.input (pos + 1) in
          let word = String.sub st.input pos (stop - pos) in
          if Hashtbl.mem reserved_words word then fail_at pos items
          else pass word stop
        else fail_at pos items)
  in
  (module struct
    let space = space
    let lexeme = lexeme
    let symbol = symbol
    let punct = punct
    let keyword = keyword
    let kw = kw
    let identifier = identifier
  end : S)
