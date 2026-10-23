open Parkour
module Prop = Parkour_test_support.Prop
module Quote = Parkour_test_support.Quote
open QCheck2

(* Generators *)

let fragments =
  [|
    "a";
    "Z";
    "7";
    "_";
    " ";
    "\n";
    "\r";
    "\t";
    "'";
    "\"";
    "\\";
    "\x00";
    "\x1f";
    "\x7f";
    "\xc3\xa9";
    "\xe6\x97\xa5";
    "\xf0\x9f\x99\x82";
    "\xe2\x80\xae";
    "\xe2\x81\xa7";
    "\xd8\x9c";
    "\xff";
    "\x80";
    "\xe6\x97";
    ";";
    "let";
  |]

let gen_string =
  Gen.(
    map (String.concat "")
      (list_size (int_bound 12)
         (map (Array.get fragments) (int_bound (Array.length fragments - 1)))))

let gen_nonempty =
  Gen.(map2 (fun a s -> a ^ s) (oneof_array fragments) gen_string)

let print_string s = Printf.sprintf "%S" s

let gen_expected =
  let small = Gen.oneof_list [ ""; "a"; "b"; "ab"; "Z"; "\xc3\xa9" ] in
  Gen.oneof
    [
      Gen.map (fun s -> Expected.Label s) small;
      Gen.map (fun s -> Expected.Literal s) small;
      Gen.return Expected.End_of_input;
    ]

let print_expected e = print_string (Expected.to_string e)
let sign n = compare n 0

(* Helpers *)

let multibyte_len s i =
  let d = String.get_utf_8_uchar s i in
  if Uchar.utf_decode_is_valid d && Uchar.utf_decode_length d >= 2 then
    Uchar.utf_decode_length d
  else 0

let word_len s i =
  match s.[i] with
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> 1
  | _ -> multibyte_len s i

let unescape body =
  let b = Buffer.create 16 in
  let n = String.length body in
  let rec go i =
    if i < n then
      if body.[i] <> '\\' then (
        Buffer.add_char b body.[i];
        go (i + 1))
      else
        match body.[i + 1] with
        | 'n' ->
            Buffer.add_char b '\n';
            go (i + 2)
        | 't' ->
            Buffer.add_char b '\t';
            go (i + 2)
        | 'r' ->
            Buffer.add_char b '\r';
            go (i + 2)
        | ('\\' | '\'' | '"') as c ->
            Buffer.add_char b c;
            go (i + 2)
        | 'x' ->
            Buffer.add_char b
              (Char.chr (int_of_string ("0x" ^ String.sub body (i + 2) 2)));
            go (i + 4)
        | 'u' ->
            let close = String.index_from body i '}' in
            let code =
              int_of_string ("0x" ^ String.sub body (i + 3) (close - i - 3))
            in
            Buffer.add_utf_8_uchar b (Uchar.of_int code);
            go (close + 1)
        | _ -> failwith "bad escape"
  in
  go 0;
  Buffer.contents b

let is_single s =
  String.length s = 1
  || (String.length s > 1 && multibyte_len s 0 = String.length s)

(* Properties *)

let span_gen =
  Gen.(
    map2
      (fun a l -> Span.make ~start:a ~stop:(a + l))
      (int_bound 50) (int_bound 50))

let span_union =
  Prop.test ~name:"Span.union is the smallest covering span"
    ~print:(fun (a, b) -> Format.asprintf "%a %a" Span.pp a Span.pp b)
    (Gen.pair span_gen span_gen)
    (fun ((a : Span.t), (b : Span.t)) ->
      let u = Span.union a b in
      u.start <= a.start && u.start <= b.start && a.stop <= u.stop
      && b.stop <= u.stop
      && (u.start = a.start || u.start = b.start)
      && (u.stop = a.stop || u.stop = b.stop)
      && Span.length u = u.stop - u.start)

let span_make =
  Prop.test ~name:"Span.make accepts exactly 0 <= start <= stop"
    ~print:QCheck2.Print.(pair int int)
    Gen.(pair (int_range (-3) 8) (int_range (-3) 8))
    (fun (start, stop) ->
      match Span.make ~start ~stop with
      | s -> start >= 0 && stop >= start && s.start = start && s.stop = stop
      | exception Invalid_argument _ -> start < 0 || stop < start)

let expected_order =
  Prop.test ~name:"Expected.compare is a total order consistent with equal"
    ~print:Print.(triple print_expected print_expected print_expected)
    Gen.(triple gen_expected gen_expected gen_expected)
    (fun (a, b, c) ->
      let cmp = Expected.compare in
      sign (cmp a b) = -sign (cmp b a)
      && Expected.equal a b = (cmp a b = 0)
      && ((not (cmp a b <= 0 && cmp b c <= 0)) || cmp a c <= 0)
      &&
      let rank = function
        | Expected.Label _ -> 0
        | Literal _ -> 1
        | End_of_input -> 2
      in
      rank a = rank b || sign (cmp a b) = sign (compare (rank a) (rank b)))

let literal_shape =
  Prop.test ~name:"Quote.literal: quoting, no raw control or invalid bytes"
    ~print:print_string gen_string (fun s ->
      let q = Quote.literal s in
      let n = String.length q in
      let q0 = if is_single s && s <> "'" then '\'' else '"' in
      n >= 2
      && q.[0] = q0
      && q.[n - 1] = q0
      && String.is_valid_utf_8 q
      && not (String.exists (fun c -> c < ' ' || c = '\x7f') q))

let literal_roundtrip =
  Prop.test ~name:"Quote.literal escapes decode back to the input"
    ~print:print_string gen_string (fun s ->
      let q = Quote.literal s in
      unescape (String.sub q 1 (String.length q - 2)) = s)

let is_bidi c =
  c = 0x061C || c = 0x200E || c = 0x200F
  || (0x202A <= c && c <= 0x202E)
  || (0x2066 <= c && c <= 0x2069)

let no_bidi =
  Prop.test ~name:"Quote: no bidi control scalar in any quoted output"
    ~print:print_string gen_string (fun s ->
      let clean q =
        let d = ref true in
        String.iteri
          (fun i _ ->
            let u = String.get_utf_8_uchar q i in
            if
              Uchar.utf_decode_is_valid u
              && is_bidi (Uchar.to_int (Uchar.utf_decode_uchar u))
            then d := false)
          q;
        !d
      in
      clean (Quote.literal s)
      && (s = "" || clean (fst (Quote.found s 0)))
      && (s = "" || clean (Quote.range s 0 (String.length s))))

let found_gen =
  Gen.(
    gen_nonempty >>= fun s ->
    map (fun i -> (s, i)) (int_bound (String.length s - 1)))

let found_props =
  Prop.test ~name:"Quote.found: length, boundaries, maximal runs, names"
    ~print:Print.(pair print_string int)
    found_gen
    (fun (s, i) ->
      let n = String.length s in
      let desc, len = Quote.found s i in
      let stop = i + len in
      let rec on_boundary k =
        if k = stop then true
        else if k > stop then false
        else on_boundary (k + max 1 (multibyte_len s k))
      in
      let c = s.[i] in
      let crlf = c = '\r' && i + 1 < n && s.[i + 1] = '\n' in
      let named =
        match c with
        | '\n' -> desc = "newline" && len = 1
        | '\r' ->
            if crlf then desc = "newline" && len = 2
            else desc = "carriage return" && len = 1
        | '\t' -> desc = "tab" && len = 1
        | ' ' -> desc = "space" && len = 1
        | _ ->
            not (List.mem desc [ "newline"; "tab"; "space"; "carriage return" ])
      in
      let maximal =
        word_len s i = 0
        || len <= 32
           && (stop = n || word_len s stop = 0 || len + word_len s stop > 32)
      in
      let special = String.contains "\n\r\t " c in
      len >= 1 && stop <= n && on_boundary i && named && maximal
      && (special
         || desc
            = Quote.literal (String.sub s i len)
              ^
              if word_len s i > 0 && stop < n && word_len s stop > 0 then
                "\xe2\x80\xa6"
              else ""))

let range_props =
  Prop.test ~name:"Quote.range: found's text if exact, else truncated literal"
    ~print:Print.(triple print_string int int)
    Gen.(
      gen_nonempty >>= fun s ->
      int_bound (String.length s - 1) >>= fun a ->
      map
        (fun b ->
          (* only ranges ending on a scalar boundary (see report) *)
          let rec boundary k =
            if k >= a + 1 + b then k
            else boundary (k + max 1 (multibyte_len s k))
          in
          (s, a, min (String.length s) (boundary a)))
        (int_bound (String.length s - a - 1)))
    (fun (s, a, b) ->
      let d, l = Quote.found s a in
      let got = Quote.range s a b in
      if a + l = b then got = d
      else
        let sub = String.sub s a (b - a) in
        let rec cut k =
          let w = max 1 (multibyte_len sub k) in
          if k + w <= 32 && k < String.length sub then cut (k + w) else k
        in
        let k = if String.length sub <= 32 then String.length sub else cut 0 in
        got
        = Quote.literal (String.sub sub 0 k)
          ^ if k < String.length sub then "\xe2\x80\xa6" else "")

let gen_diag =
  Gen.(
    map3
      (fun sev ex (code, span) -> (sev, ex, code, span))
      bool
      (list_size (int_bound 8) gen_expected)
      (pair (option (oneof_list [ "E42"; "W1" ])) span_gen))

let diag_props =
  Prop.test ~name:"Diagnostic.make: sorted dedup expected, compact pp format"
    ~print:(fun (_, ex, _, _) -> Print.list print_expected ex)
    gen_diag
    (fun (is_err, ex, code, span) ->
      let severity = if is_err then `Error else `Warning in
      let d = Diagnostic.make ~severity ?code ~expected:ex ~span "msg" in
      let got = Diagnostic.expected d in
      let rec strictly_sorted = function
        | a :: (b :: _ as r) -> Expected.compare a b < 0 && strictly_sorted r
        | _ -> true
      in
      strictly_sorted got
      && List.for_all (fun e -> List.exists (Expected.equal e) got) ex
      && List.for_all (fun e -> List.exists (Expected.equal e) ex) got
      && Diagnostic.is_error d = is_err
      && Format.asprintf "%a" Diagnostic.pp d
         = Printf.sprintf "%s: %s%s: msg"
             (if span.start = span.stop then Printf.sprintf "byte %d" span.start
              else Printf.sprintf "bytes %d-%d" span.start span.stop)
             (if is_err then "error" else "warning")
             (match code with None -> "" | Some c -> "[" ^ c ^ "]"))

let () =
  Prop.run "support_modules"
    [
      span_union;
      span_make;
      expected_order;
      literal_shape;
      literal_roundtrip;
      no_bidi;
      found_props;
      range_props;
      diag_props;
    ]
