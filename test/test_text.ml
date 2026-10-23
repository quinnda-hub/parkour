(* Properties of the text primitives: character classes, numbers, strings,
   Lexeme and Utf8. *)

module P = Parkour
module D = P.Diagnostic
module Prop = Parkour_test_support.Prop
module G = Parkour_test_support.Grammar
open QCheck2

(* Observations *)

type 'a r = V of 'a * int | E of D.t

let run p s =
  match P.parse_prefix p s with
  | Ok (v, pos) -> V (v, pos)
  | Error (d :: _) -> E d
  | Error [] -> failwith "no diagnostic"

(* [d] is the custom error [msg] underlining [start, stop). *)
let custom msg start stop = function
  | E d ->
      let sp = D.span d in
      D.message d = msg && sp.start = start && sp.stop = stop
  | V _ -> false

(* [r] fails at [pos] expecting exactly [items]. *)
let expects ?(pos = 0) items = function
  | E d -> D.expected d = items && (D.span d).start = pos
  | V _ -> false

let pp_bytes s = Printf.sprintf "%S" s
let exhaustive name f = Prop.test ~count:1 ~name Gen.unit f
let all_bytes f = List.for_all f (List.init 256 Char.chr)

(* Generators *)

let utf8 u =
  let b = Buffer.create 4 in
  Buffer.add_utf_8_uchar b u;
  Buffer.contents b

let gen_scalar =
  let range lo hi = Gen.map Uchar.of_int (Gen.int_range lo hi) in
  Gen.oneof_weighted
    [
      (5, range 0x20 0x7e);
      ( 2,
        Gen.oneof_list (List.map Uchar.of_int [ 0x22; 0x5c; 0x2f; 0x0a; 0x0d ])
      );
      (2, range 0 0x1f);
      (2, range 0x80 0x7ff);
      (2, range 0x800 0xd7ff);
      (1, range 0xe000 0xffff);
      (2, range 0x10000 0x10ffff);
    ]

let gen_scalars = Gen.list_size (Gen.int_range 0 12) gen_scalar
let encode_scalars l = String.concat "" (List.map utf8 l)

(* Arbitrary bytes, weighted towards UTF-8 lead and continuation bytes, with
   and without a well-formed prefix. *)
let gen_bytes =
  let byte lo hi = Gen.map Char.chr (Gen.int_range lo hi) in
  let raw =
    Gen.string_size
      ~gen:
        (Gen.oneof_weighted
           [
             (4, Gen.char_range 'a' 'z');
             (3, byte 0x80 0xbf);
             (3, byte 0xc0 0xff);
           ])
      (Gen.int_range 0 8)
  in
  Gen.oneof [ raw; Gen.map2 (fun l r -> encode_scalars l ^ r) gen_scalars raw ]

(* A reference decoder: the scalars before the first invalid byte, and where
   that byte is. *)
let naive s =
  let n = String.length s in
  let rec go i acc =
    if i >= n then (List.rev acc, None)
    else
      let d = String.get_utf_8_uchar s i in
      if Uchar.utf_decode_is_valid d then
        go (i + Uchar.utf_decode_length d) (Uchar.utf_decode_uchar d :: acc)
      else (List.rev acc, Some i)
  in
  go 0 []

(* Character classes *)

let ref_class = function
  | "digit" -> fun c -> c >= '0' && c <= '9'
  | "letter" -> fun c -> (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
  | "hexadecimal digit" ->
      fun c ->
        (c >= '0' && c <= '9')
        || (c >= 'a' && c <= 'f')
        || (c >= 'A' && c <= 'F')
  | _ -> fun c -> String.trim (String.make 1 c) = "" && c <> '\x0b'

let classes =
  [
    ("digit", P.is_digit, P.digit);
    ("letter", P.is_letter, P.letter);
    ("hexadecimal digit", P.is_hex_digit, P.hex_digit);
  ]

let class_parsers =
  exhaustive "char classes: parser accepts iff the predicate holds" (fun () ->
      List.for_all
        (fun (name, pred, p) ->
          all_bytes (fun c ->
              let expected = ref_class name c in
              pred c = expected
              &&
              match run p (String.make 1 c) with
              | V (c', 1) -> expected && c' = c
              | r -> (not expected) && expects [ P.Expected.Label name ] r))
        classes)

let class_predicates =
  exhaustive "char classes: is_alnum and is_space match their definitions"
    (fun () ->
      all_bytes (fun c ->
          P.is_alnum c = (ref_class "digit" c || ref_class "letter" c)
          && P.is_space c = ref_class "" c))

let gen_set = Gen.string_size ~gen:(Gen.char_range 'a' 'f') (Gen.int_range 0 4)

let one_none_of =
  Prop.test ~name:"one_of / none_of agree with String.contains"
    ~print:(fun (set, c) -> Printf.sprintf "%S %C" set c)
    Gen.(pair gen_set (char_range 'a' 'h'))
    (fun (set, c) ->
      let s = String.make 1 c and inside = String.contains set c in
      let literals =
        List.sort_uniq P.Expected.compare
          (List.init (String.length set) (fun i ->
               P.Expected.Literal (String.make 1 set.[i])))
      in
      (match run (P.one_of set) s with
        | V (c', 1) -> inside && c' = c
        | r -> (not inside) && expects literals r)
      &&
      match run (P.none_of set) s with
      | V (c', 1) -> (not inside) && c' = c
      | r -> inside && expects [] r)

let none_of_at_end =
  exhaustive "none_of fails at end of input expecting nothing" (fun () ->
      expects [] (run (P.none_of "a") ""))

let newline_whitespace =
  Prop.test ~name:"newline takes \\n or \\r\\n; whitespace is hidden"
    ~print:pp_bytes
    Gen.(
      string_size
        ~gen:(oneof_list [ ' '; '\t'; '\n'; '\r'; '\x0c' ])
        (int_range 0 6))
    (fun ws ->
      let n = String.length ws in
      (match run P.whitespace (ws ^ "b") with
        | V ((), pos) -> pos = n
        | _ -> false)
      && expects ~pos:n [ P.Expected.Literal "a" ]
           (run P.(whitespace *> char 'a') (ws ^ "b"))
      && (match run P.newline "\r\nx" with V ((), 2) -> true | _ -> false)
      && (match run P.newline "\nx" with V ((), 1) -> true | _ -> false)
      && expects [ P.Expected.Label "newline" ] (run P.newline "\rx")
      && expects [ P.Expected.Label "newline" ] (run P.newline ""))

(* string_ci and consumed *)

let string_ci_prop =
  let alpha = Gen.oneof_list [ 'a'; 'b'; 'A'; 'B'; '1' ] in
  let word = Gen.string_size ~gen:alpha (Gen.int_range 0 4) in
  let flip = Gen.map String.uppercase_ascii word in
  Prop.test ~name:"string_ci matches iff ASCII-case-insensitively equal"
    ~print:(fun (l, s) -> Printf.sprintf "%S on %S" l s)
    Gen.(pair word (oneof [ word; map2 ( ^ ) flip word; map2 ( ^ ) word flip ]))
    (fun (lit, s) ->
      let n = String.length lit in
      let ok =
        String.length s >= n
        && String.lowercase_ascii (String.sub s 0 n)
           = String.lowercase_ascii lit
      in
      match run (P.string_ci lit) s with
      | V (v, pos) -> ok && v = String.sub s 0 n && pos = n
      | r -> (not ok) && expects [ P.Expected.Literal lit ] r)

let consumed_prop =
  Prop.test ~name:"consumed p is the text spanned p reports"
    ~print:(fun (g, s) -> Printf.sprintf "%s on %S" (G.to_string g) s)
    Gen.(pair (G.gen ~cuts:false) G.gen_input)
    (fun (g, s) ->
      let p = G.compile g in
      let messages ds = List.map D.message ds in
      match
        (P.parse_prefix (P.spanned p) s, P.parse_prefix (P.consumed p) s)
      with
      | Ok (((_ : string), (sp : P.Span.t)), pos), Ok (text, pos') ->
          pos = pos' && text = String.sub s sp.start (sp.stop - sp.start)
      | Error a, Error b -> messages a = messages b
      | _ -> false
      | exception P.Grammar_error _ -> true)

(* Numbers *)

let digits_only s = s <> "" && String.for_all (fun c -> c >= '0' && c <= '9') s

(* Whole-string validators, written from the grammar rather than the scanner;
   the reference match is the longest prefix they accept. *)
let is_natural = digits_only

let is_integer s =
  if s <> "" && s.[0] = '-' then
    digits_only (String.sub s 1 (String.length s - 1))
  else digits_only s

let is_number s =
  let split c s =
    match String.index_opt s c with
    | None -> (s, None)
    | Some i ->
        (String.sub s 0 i, Some (String.sub s (i + 1) (String.length s - i - 1)))
  in
  let mantissa, exp =
    match split 'e' s with
    | m, None -> split 'E' m |> fun (m, e) -> (m, e)
    | m, e -> (m, e)
  in
  let signed e =
    e <> ""
    && (e.[0] = '+' || e.[0] = '-')
    && digits_only (String.sub e 1 (String.length e - 1))
  in
  let exp_ok =
    match exp with None -> true | Some e -> digits_only e || signed e
  in
  let int_part, frac = split '.' mantissa in
  let frac_ok = match frac with None -> true | Some f -> digits_only f in
  exp_ok && frac_ok && is_integer int_part

let longest valid s =
  let rec go l =
    if l = 0 then None
    else if valid (String.sub s 0 l) then Some l
    else go (l - 1)
  in
  go (String.length s)

let gen_numeric =
  Gen.string_size
    ~gen:(Gen.oneof_list [ '0'; '1'; '9'; '.'; '-'; '+'; 'e'; 'E'; 'x' ])
    (Gen.int_range 0 8)

let longest_match name label range valid conv p =
  Prop.test ~name:(name ^ ": longest match, value of the consumed text")
    ~print:pp_bytes gen_numeric (fun s ->
      match (longest valid s, run p s) with
      | Some l, r -> (
          match conv (String.sub s 0 l) with
          | Some v -> r = V (v, l)
          | None -> custom range 0 l r)
      | None, r -> expects [ P.Expected.Label label ] r)

let roundtrips =
  [
    Prop.test ~name:"natural roundtrips string_of_int" ~print:string_of_int
      Gen.nat (fun n -> P.parse P.natural (string_of_int n) = Ok n);
    Prop.test ~name:"integer roundtrips string_of_int" ~print:string_of_int
      Gen.int (fun n -> P.parse P.integer (string_of_int n) = Ok n);
    Prop.test ~name:"number roundtrips finite floats printed with %.17g"
      ~print:(Printf.sprintf "%h")
      (Gen.map (fun f -> if Float.is_finite f then f else 0.5) Gen.float)
      (fun f ->
        let s = Printf.sprintf "%.17g" f in
        is_number s && P.parse P.number s = Ok f);
  ]

let gen_digits lo hi =
  Gen.string_size ~gen:(Gen.char_range '0' '9') (Gen.int_range lo hi)

(* [p] on [tok ^ rest] agrees with [int_of_string_opt tok]. *)
let agrees_with_int_of_string p tok rest =
  let n = String.length tok in
  match (int_of_string_opt tok, run p (tok ^ rest)) with
  | Some v, V (v', pos) -> v = v' && pos = n
  | None, r -> custom "integer out of range" 0 n r
  | Some _, E _ -> false

let int_range_prop =
  Prop.test ~name:"integer / natural out of range: message, token span"
    ~print:(fun (neg, d, rest) -> Printf.sprintf "%b %s %S" neg d rest)
    Gen.(triple bool (gen_digits 1 24) (oneof_list [ ""; " x"; ".5" ]))
    (fun (neg, d, rest) ->
      let tok = (if neg then "-" else "") ^ d in
      agrees_with_int_of_string P.integer tok rest
      && agrees_with_int_of_string P.natural d rest)

(* Digit strings near [max_int] and [min_int] (one digit changed, some leading
   zeros), where an accumulating parser could go wrong by one. *)
let int_boundary_prop =
  let gen_near =
    Gen.(
      let* base = oneof_list [ max_int; min_int; max_int / 10; min_int / 10 ] in
      let digits =
        let s = string_of_int base in
        if base < 0 then String.sub s 1 (String.length s - 1) else s
      in
      let* i = int_range 0 (String.length digits - 1) in
      let* d = char_range '0' '9' in
      let* zeros = int_range 0 2 in
      let bytes = Bytes.of_string digits in
      Bytes.set bytes i d;
      return
        ( base < 0,
          String.make zeros '0' ^ Bytes.to_string bytes,
          if base < 0 then "-" else "" ))
  in
  Prop.test ~name:"integer / natural near the int boundaries: int_of_string_opt"
    ~print:(fun (_, d, sign) -> sign ^ d)
    gen_near
    (fun (_, d, sign) ->
      agrees_with_int_of_string P.integer (sign ^ d) ""
      && agrees_with_int_of_string P.natural d "")

let float_range_prop =
  Prop.test ~name:"number out of range: message, token span"
    ~print:(fun (d, e) -> Printf.sprintf "%se%d" d e)
    Gen.(pair (gen_digits 1 3) (int_range 290 330))
    (fun (d, e) ->
      let tok = Printf.sprintf "-%s.5E+%d" d e in
      let n = String.length tok in
      match
        (Float.is_finite (float_of_string tok), run P.number (tok ^ "!"))
      with
      | true, V (_, pos) -> pos = n
      | false, r -> custom "number out of range" 0 n r
      | true, E _ -> false)

(* Strings *)

let short_escape c =
  match c with
  | 0x22 -> Some "\\\""
  | 0x5c -> Some "\\\\"
  | 0x2f -> Some "\\/"
  | 0x08 -> Some "\\b"
  | 0x0c -> Some "\\f"
  | 0x0a -> Some "\\n"
  | 0x0d -> Some "\\r"
  | 0x09 -> Some "\\t"
  | _ -> None

(* One scalar as JSON text, in a form picked by [sel] among those legal. *)
let encode_json (u, sel) =
  let c = Uchar.to_int u in
  let hex upper n = Printf.sprintf (if upper then "\\u%04X" else "\\u%04x") n in
  let uescape upper =
    if c < 0x10000 then hex upper c
    else
      let v = c - 0x10000 in
      hex upper (0xd800 + (v lsr 10)) ^ hex upper (0xdc00 + (v land 0x3ff))
  in
  let raw =
    if List.mem c [ 0x22; 0x5c; 0x0a; 0x0d ] then None else Some (utf8 u)
  in
  let forms =
    List.filter_map Fun.id
      [ raw; short_escape c; Some (uescape false); Some (uescape true) ]
  in
  List.nth forms (sel mod List.length forms)

let gen_json_body =
  Gen.(list_size (int_range 0 10) (pair gen_scalar (int_bound 3)))

let json_text body = String.concat "" (List.map encode_json body)
let print_body body = Printf.sprintf "%S" (json_text body)

let string_roundtrip =
  Prop.test ~name:"string_literal returns the original, whatever the escapes"
    ~print:print_body gen_json_body (fun body ->
      let text = "\"" ^ json_text body ^ "\"" in
      run P.string_literal (text ^ "tail")
      = V (encode_scalars (List.map fst body), String.length text))

let string_unterminated =
  Prop.test ~name:"string_literal: newline or end of input is unterminated"
    ~print:(fun (b, t) -> Printf.sprintf "%s + %S" (print_body b) t)
    Gen.(pair gen_json_body (oneof_list [ ""; "\n"; "\r"; "\nxyz"; "\\" ]))
    (fun (body, tail) ->
      let n = String.length (json_text body) in
      let stop = if tail = "\\" then n + 2 else n + 1 in
      custom "unterminated string" 0 stop
        (run P.string_literal ("\"" ^ json_text body ^ tail)))

let string_bad_escape =
  Prop.test
    ~name:"string_literal: unknown escapes underline backslash and character"
    ~print:(fun (b, u) -> Printf.sprintf "%s + %S" (print_body b) (utf8 u))
    Gen.(
      pair gen_json_body
        (map
           (fun u ->
             if String.contains "\"\\/bfnrtu" (utf8 u).[0] then
               Uchar.of_char 'q'
             else u)
           gen_scalar))
    (fun (body, u) ->
      let start = 1 + String.length (json_text body) in
      custom "invalid escape sequence" start
        (start + 1 + String.length (utf8 u))
        (run P.string_literal ("\"" ^ json_text body ^ "\\" ^ utf8 u ^ "\"")))

let string_bad_unicode =
  let hexd = Gen.oneof_list [ '0'; '7'; 'a'; 'F' ] in
  let bad =
    Gen.oneof
      [
        (* fewer than four hex digits, then something else *)
        Gen.map2
          (fun h c -> ("\\u" ^ h ^ String.make 1 c, 2 + String.length h))
          (Gen.string_size ~gen:hexd (Gen.int_range 0 3))
          (Gen.oneof_list [ 'x'; '"'; '-' ]);
        Gen.map
          (fun n -> (Printf.sprintf "\\u%04x" n, 6))
          (Gen.int_range 0xdc00 0xdfff);
        Gen.map2
          (fun n (tail, len) -> (Printf.sprintf "\\u%04X%s" n tail, len))
          (Gen.int_range 0xd800 0xdbff)
          (Gen.oneof_list
             [
               ("\"", 6);
               ("x", 6);
               ("\\n", 6);
               ("\\u0041", 12);
               ("\\ud800", 12);
               ("\\u12\"", 6);
             ]);
      ]
  in
  Prop.test ~name:"string_literal: malformed or unpaired \\u escapes"
    ~print:(fun (b, (t, _)) -> Printf.sprintf "%s + %S" (print_body b) t)
    Gen.(pair gen_json_body bad)
    (fun (body, (text, len)) ->
      let start = 1 + String.length (json_text body) in
      custom "invalid unicode escape" start (start + len)
        (run P.string_literal ("\"" ^ json_text body ^ text)))

let quoted_without_escape =
  Prop.test ~name:"quoted without escape copies raw bytes" ~print:pp_bytes
    Gen.(string_size ~gen:(map Char.chr (int_range 0 255)) (int_range 0 10))
    (fun s ->
      let body = String.concat "" (String.split_on_char '\'' s) in
      let body = String.concat "" (String.split_on_char '\n' body) in
      let body = String.concat "" (String.split_on_char '\r' body) in
      run (P.quoted '\'') ("'" ^ body ^ "'x") = V (body, String.length body + 2))

let strip chars s =
  String.concat ""
    (List.map (String.make 1)
       (List.filter
          (fun c -> not (String.contains chars c))
          (List.of_seq (String.to_seq s))))

let quoted_options =
  Prop.test
    ~name:
      "quoted ~doubled ~multiline: roundtrips; unterminated multiline \
       underlines the opener"
    ~print:(fun (d, m, b) -> Printf.sprintf "doubled=%b multiline=%b %S" d m b)
    Gen.(
      triple bool bool
        (string_size
           ~gen:(oneof_list [ 'a'; '\''; '\n'; '\r'; ' ' ])
           (int_range 0 10)))
    (fun (doubled, multiline, s) ->
      let body = if multiline then s else strip "\n\r" s in
      let body = if doubled then body else strip "'" body in
      let enc =
        if doubled then String.concat "''" (String.split_on_char '\'' body)
        else body
      in
      let text = "'" ^ enc ^ "'" in
      let p = P.quoted ~doubled ~multiline '\'' in
      run p (text ^ "x") = V (body, String.length text)
      &&
      (* No closing quote: an open pair of quotes does not close it either. *)
      let open_ = strip "'" body in
      let tail = if doubled then "''" else "" in
      let src = "'" ^ open_ ^ tail in
      if multiline then custom "unterminated string" 0 1 (run p src)
      else
        let stop =
          match
            (String.index_from_opt src 1 '\n', String.index_from_opt src 1 '\r')
          with
          | Some a, Some b -> min a b
          | Some a, None | None, Some a -> a
          | None, None -> String.length src
        in
        custom "unterminated string" 0 stop (run p src))

(* Lexeme *)

type policy = { op : string; cl : string; line : string; nested : bool }

let flat = { op = "/*"; cl = "*/"; line = "#"; nested = false }
let nesting = { op = "(*"; cl = "*)"; line = "//"; nested = true }

(* The line opener is a prefix of the block opener. *)
let lua = { op = "--[["; cl = "]]"; line = "--"; nested = false }

let make_lexeme ?(newlines = true) p =
  P.Lexeme.make ~line_comments:[ p.line ]
    ~block_comments:[ (p.op, p.cl) ]
    ~nested_comments:p.nested ~newlines ()

let gen_ws ~newlines =
  Gen.string_size
    ~gen:
      (Gen.oneof_list
         (if newlines then [ ' '; '\t'; '\n'; '\r'; '\x0c' ]
          else [ ' '; '\t'; '\x0c' ]))
    (Gen.int_range 1 3)

(* The inside of a block comment; the line opener is inert in it. *)
let rec gen_parts depth p =
  let frag = Gen.oneof_list [ "a"; " "; "\n"; "\t"; p.line ] in
  let part =
    if p.nested && depth > 0 then
      Gen.oneof_weighted [ (4, frag); (1, gen_block (depth - 1) p) ]
    else frag
  in
  Gen.map (String.concat "") (Gen.list_size (Gen.int_range 0 4) part)

and gen_block depth p =
  Gen.map (fun parts -> p.op ^ parts ^ p.cl) (gen_parts depth p)

let gen_line p =
  Gen.map2
    (fun body nl -> p.line ^ body ^ nl)
    (Gen.map (String.concat "")
       (Gen.list_size (Gen.int_range 0 3)
          (Gen.oneof_list [ "a"; " "; "/*"; "*/"; "(*"; "\t" ])))
    (Gen.oneof_list [ "\n"; "\r\n" ])

let gen_trivia p =
  Gen.map (String.concat "")
    (Gen.list_size (Gen.int_range 0 5)
       (Gen.oneof_weighted
          [ (3, gen_ws ~newlines:true); (2, gen_line p); (2, gen_block 2 p) ]))

let space_prop p name =
  Prop.test
    ~name:("Lexeme.space consumes exactly trivia (" ^ name ^ ")")
    ~print:(fun (t, e) -> Printf.sprintf "%S + %S" t e)
    Gen.(pair (gen_trivia p) (oneof_list [ "x"; "" ]))
    (fun (trivia, stop) ->
      let module L = (val make_lexeme p) in
      let n = String.length trivia in
      (match run L.space (trivia ^ stop) with
        | V ((), pos) -> pos = n
        | _ -> false)
      && (match run (L.symbol "+") ("+" ^ trivia ^ stop) with
        | V ("+", pos) -> pos = n + 1
        | _ -> false)
      && List.for_all
           (fun s -> run (L.punct "+") s = run P.(map ignore (L.symbol "+")) s)
           [ "+" ^ trivia ^ stop; trivia ^ stop ]
      && expects ~pos:n [ P.Expected.Literal "x" ]
           (run P.(L.space *> char 'x') (trivia ^ "y")))

let space_no_newlines =
  Prop.test ~name:"Lexeme.space ~newlines:false stops at the newline"
    ~print:(fun (t, l, e) -> Printf.sprintf "%S + %S + %S" t l e)
    Gen.(
      triple
        (map (String.concat "")
           (list_size (int_range 0 4)
              (oneof_weighted
                 [ (2, gen_ws ~newlines:false); (2, gen_block 0 flat) ])))
        (oneof_list [ ""; "# comment" ])
        (oneof_list [ "\nx"; "\r\nx"; "" ]))
    (fun (trivia, line, stop) ->
      let module L = (val make_lexeme ~newlines:false flat) in
      match run L.space (trivia ^ line ^ stop) with
      | V ((), pos) -> pos = String.length trivia + String.length line
      | _ -> false)

let space_unterminated p name =
  Prop.test
    ~name:("Lexeme.space: unterminated block comment (" ^ name ^ ")")
    ~print:(fun (t, b) -> Printf.sprintf "%S + %S" t b)
    Gen.(pair (gen_trivia p) (gen_parts 2 p))
    (fun (trivia, parts) ->
      let module L = (val make_lexeme p) in
      let n = String.length trivia in
      custom "unterminated comment" n
        (n + String.length p.op)
        (run L.space (trivia ^ p.op ^ parts)))

let keyword_prop =
  Prop.test
    ~name:"keyword k succeeds on k^c iff c is not an identifier character"
    ~print:(fun (k, c) -> Printf.sprintf "%S %S" k c)
    Gen.(
      pair
        (oneof_list [ "if"; "a1"; "+"; "_" ])
        (oneof_list [ ""; "a"; "1"; "_"; " "; "("; "+"; "\xc3\xa9" ]))
    (fun (k, c) ->
      let module L = (val P.Lexeme.make ()) in
      let ident_char = c <> "" && (P.is_alnum c.[0] || c.[0] = '_') in
      match run (L.keyword k) (k ^ c) with
      | V (v, _) -> (not ident_char) && v = k
      | r -> ident_char && expects [ P.Expected.Literal k ] r)

let identifier_prop =
  let reserved = [ "a"; "ab"; "b1"; "_" ] in
  Prop.test ~name:"identifier rejects exactly the reserved words"
    ~print:pp_bytes
    Gen.(string_size ~gen:(oneof_list [ 'a'; 'b'; '1'; '_' ]) (int_range 1 3))
    (fun w ->
      let module L = (val P.Lexeme.make ~reserved ()) in
      let starts_digit = w.[0] = '1' in
      match run L.identifier (w ^ " !") with
      | V (v, pos) ->
          (not starts_digit)
          && (not (List.mem w reserved))
          && v = w
          && pos = String.length w + 1
      | r ->
          (starts_digit || List.mem w reserved)
          && expects [ P.Expected.Label "identifier" ] r)

let make_validation =
  exhaustive "Lexeme.make rejects empty and ambiguous delimiters" (fun () ->
      let bad f =
        match f () with exception Invalid_argument _ -> true | _ -> false
      in
      bad (fun () -> P.Lexeme.make ~line_comments:[ "" ] ())
      && bad (fun () -> P.Lexeme.make ~block_comments:[ ("", "*/") ] ())
      && bad (fun () -> P.Lexeme.make ~block_comments:[ ("/*", "") ] ())
      && bad (fun () ->
          P.Lexeme.make ~block_comments:[ ("|", "|") ] ~nested_comments:true ())
      && not (bad (fun () -> P.Lexeme.make ~block_comments:[ ("|", "|") ] ())))

(* Utf8 *)

let utf8_any_valid =
  Prop.test ~name:"Utf8.any: many yields the scalars of a naive decode"
    ~print:(fun l -> pp_bytes (encode_scalars l))
    gen_scalars
    (fun l ->
      let s = encode_scalars l in
      fst (naive s) = l && P.parse (P.many P.Utf8.any) s = Ok l)

let utf8_any_invalid =
  Prop.test ~name:"Utf8.any: the first invalid byte is reported" ~print:pp_bytes
    gen_bytes (fun s ->
      let scalars, bad = naive s in
      let p = P.many P.Utf8.any in
      match bad with
      | None -> run p s = V (scalars, String.length s)
      | Some i ->
          run p s = V (scalars, i)
          && custom "invalid UTF-8" i (i + 1)
               (match P.parse p s with Error (d :: _) -> E d | _ -> V ((), 0)))

let utf8_single =
  Prop.test ~name:"Utf8.any / char / satisfy on the first scalar"
    ~print:pp_bytes gen_bytes (fun s ->
      let e = Uchar.of_int 0xe9 in
      let ps =
        [
          P.Utf8.any;
          P.Utf8.char e;
          P.Utf8.satisfy "x" (fun u -> Uchar.to_int u < 0x80);
        ]
      in
      s = ""
      ||
      let first = String.get_utf_8_uchar s 0 in
      List.for_all2
        (fun p accepts ->
          match run p s with
          | V (u, pos) ->
              Uchar.utf_decode_is_valid first
              && accepts u
              && Uchar.equal u (Uchar.utf_decode_uchar first)
              && pos = Uchar.utf_decode_length first
          | r ->
              if Uchar.utf_decode_is_valid first then
                (not (accepts (Uchar.utf_decode_uchar first)))
                && not (custom "invalid UTF-8" 0 1 r)
              else custom "invalid UTF-8" 0 1 r)
        ps
        [ (fun _ -> true); Uchar.equal e; (fun u -> Uchar.to_int u < 0x80) ])

let utf8_take_while =
  Prop.test
    ~name:"Utf8.take_while / take_while1 / skip_while agree with a naive scan"
    ~print:(fun (s, t) -> Printf.sprintf "%S below %x" s t)
    Gen.(pair gen_bytes (oneof_list [ 0x41; 0x80; 0x800; 0x10000 ]))
    (fun (s, limit) ->
      let pred u = Uchar.to_int u < limit in
      let scalars, bad = naive s in
      let rec prefix = function
        | u :: rest when pred u -> u :: prefix rest
        | _ -> []
      in
      let taken = prefix scalars in
      let text = encode_scalars taken in
      let stop = String.length text in
      let at_bad = bad = Some stop in
      run (P.Utf8.take_while pred) s = V (text, stop)
      && run (P.Utf8.skip_while pred) s = V ((), stop)
      && (match run (P.Utf8.take_while1 "one" pred) s with
        | V (v, pos) -> stop > 0 && v = text && pos = stop
        | r ->
            stop = 0
            &&
            if at_bad then custom "invalid UTF-8" 0 1 r
            else expects [ P.Expected.Label "one" ] r)
      &&
      (* what follows reports the invalid byte if that is where it stopped *)
      match run P.(Utf8.take_while pred *> char '!') s with
      | V _ -> false
      | E d ->
          if at_bad then D.message d = "invalid UTF-8"
          else D.message d <> "invalid UTF-8")

(* Robustness. Many consecutive comments, Utf8 and tokenize at 10^6:
   see test_robustness.ml. *)

let stack_safety =
  Prop.test ~count:1 ~name:"stack safety on 10 MB inputs" Gen.unit (fun () ->
      let big = 10_000_000 in
      let module L =
        (val P.Lexeme.make ~line_comments:[ "#" ]
               ~block_comments:[ ("(*", "*)") ]
               ~nested_comments:true ())
      in
      let ok p text n =
        match run p text with V (_, pos) -> pos = n | E _ -> false
      in
      let quote body = "\"" ^ body ^ "\"" in
      let a = String.make big 'a' in
      let escapes =
        String.concat "" (List.init (big / 10) (fun _ -> "\\n\\u00e9 "))
      in
      let deep =
        String.concat "" (List.init (big / 10) (fun _ -> "(*"))
        ^ String.make (big / 10) ' '
        ^ String.concat "" (List.init (big / 10) (fun _ -> "*)"))
      in
      ok P.string_literal (quote a) (big + 2)
      && ok P.string_literal (quote escapes) (String.length escapes + 2)
      && ok L.space ("# " ^ a) (big + 2)
      && ok L.space ("(*" ^ a ^ "*)") (big + 4)
      && ok L.space deep (String.length deep)
      && ok L.identifier a big
      && ok (P.Utf8.take_while (fun _ -> true)) a big
      && ok (P.consumed (P.take_while (fun _ -> true))) a big)

let () =
  Prop.run "text"
    ([
       class_parsers;
       class_predicates;
       one_none_of;
       none_of_at_end;
       newline_whitespace;
       string_ci_prop;
       consumed_prop;
       longest_match "natural" "integer" "integer out of range" is_natural
         int_of_string_opt P.natural;
       longest_match "integer" "integer" "integer out of range" is_integer
         int_of_string_opt P.integer;
       longest_match "number" "number" "number out of range" is_number
         (fun s ->
           let f = float_of_string s in
           if Float.is_finite f then Some f else None)
         P.number;
     ]
    @ roundtrips
    @ [
        int_range_prop;
        int_boundary_prop;
        float_range_prop;
        string_roundtrip;
        string_unterminated;
        string_bad_escape;
        string_bad_unicode;
        quoted_without_escape;
        quoted_options;
        space_prop flat "flat";
        space_prop nesting "nested";
        space_prop lua "line opener prefixes block opener";
        space_no_newlines;
        space_unterminated flat "flat";
        space_unterminated nesting "nested";
        space_unterminated lua "line opener prefixes block opener";
        keyword_prop;
        identifier_prop;
        make_validation;
        utf8_any_valid;
        utf8_any_invalid;
        utf8_single;
        utf8_take_while;
        stack_safety;
      ])
