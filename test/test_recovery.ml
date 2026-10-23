(* Properties of error recovery: surviving declarations, termination, the
   result contract of [parse]/[parse_partial], transactional diagnostics,
   recorded contexts, the skipping parsers, [max_errors], dedupe and
   [or_insert]. *)

module P = Parkour
module D = P.Diagnostic
module Prop = Parkour_test_support.Prop
open QCheck2

let errors ds = List.filter D.is_error ds
let start_of d = (D.span d).start

(* Corrupting one declaration *)

let is_lower c = c >= 'a' && c <= 'z'

(* [name = 123;] with a cut after [=]. *)
let decl =
  let open P in
  let+ name = take_while1 "name" is_lower
  and+ () = char '=' *> cut
  and+ v = natural <* char ';' in
  Some (name, v)

let decls =
  P.(many (recover ~sync:skip_line ~fallback:(fun _ -> None) (decl <* newline)))

let gen_decls =
  Gen.(
    list_size (Prop.range 1 6)
      (pair
         (string_size ~gen:(char_range 'a' 'z') (int_range 1 4))
         (int_bound 999)))

let tails = [ ""; "x;"; "12"; "1 2;"; ";"; "9999999999999999999999;"; "-5;" ]

let corrupt_one =
  Prop.test ~name:"corrupting one declaration loses only that declaration"
    ~print:(fun (ds, i, t) ->
      Printf.sprintf "%s corrupt #%d with %S"
        (Print.(list (pair string int)) ds)
        i t)
    Gen.(
      let* ds = gen_decls in
      let* i = int_bound (List.length ds - 1) in
      let* t = oneof_list tails in
      return (ds, i, t))
    (fun (ds, i, t) ->
      let line j (n, v) =
        if j = i then n ^ "=" ^ t ^ "\n" else Printf.sprintf "%s=%d;\n" n v
      in
      let input = String.concat "" (List.mapi line ds) in
      let line_start =
        List.fold_left ( + ) 0
          (List.mapi
             (fun j d -> if j < i then String.length (line j d) else 0)
             ds)
      in
      let line_stop = line_start + String.length (line i (List.nth ds i)) in
      let expected =
        List.mapi (fun j (n, v) -> if j = i then None else Some (n, v)) ds
      in
      let value, log = P.parse_partial decls input in
      value = Some expected
      && errors log <> []
      && List.for_all
           (fun d ->
             let s = D.span d in
             s.start >= line_start && s.stop <= line_stop)
           log
      && Result.is_error (P.parse decls input))

(* Termination and the result contract on arbitrary input *)

type stmt = Assign | Block of stmt list | Bad

let program =
  let open P in
  let module L = (val Lexeme.make ()) in
  let sync = skip_past ~nested:(char '{', char '}') (char ';') in
  let item p = recover ~catch:`Any ~sync ~fallback:(fun _ -> Bad) p in
  let stmt =
    fix (fun stmt ->
        let block =
          let+ l = char '{' *> cut *> many (item stmt) <* char '}' in
          Block l
        in
        let assign =
          let+ _ =
            take_while1 "name" is_lower
            *> char '=' *> cut
            *> (block <|> (char 'a' >>| fun _ -> Assign))
            <* char ';'
          in
          Assign
        in
        block <|> assign)
  in
  many (item stmt)

let gen_noise =
  Gen.(
    string_size
      ~gen:(oneof_list [ 'a'; '='; '{'; '}'; ';'; ' ' ])
      (Prop.bound 40))

let terminates =
  Prop.test ~name:"recovery terminates; Some means finished, None means stopped"
    ~print:(fun (s, k) -> Printf.sprintf "%S max_errors=%d" s k)
    Gen.(pair gen_noise (int_range 1 6))
    (fun (input, k) ->
      match P.parse_partial ~max_errors:k program input with
      | exception _ -> false
      | value, log ->
          let n = List.length (errors log) in
          n <= k
          && (value <> None || n >= 1)
          && Result.is_ok (P.parse ~max_errors:k program input)
             = (value <> None && n = 0))

(* Emitted diagnostics against [parse] *)

let gen_severity = Gen.oneof_list [ `Error; `Warning ]

let diag severity start =
  D.make ~severity ~span:(P.Span.make ~start ~stop:start) "emitted"

let emit_contract =
  Prop.test ~name:"parse is Ok iff no error was logged; warnings stay visible"
    ~print:(fun (g, s, sev, first) ->
      Printf.sprintf "%s on %S, emit %s %s"
        (Parkour_test_support.Grammar.to_string g)
        s
        (if sev = `Error then "error" else "warning")
        (if first then "before" else "after"))
    Gen.(
      quad
        (Parkour_test_support.Grammar.gen ~cuts:true)
        Parkour_test_support.Grammar.gen_input gen_severity bool)
    (fun (g, s, sev, first) ->
      let p = Parkour_test_support.Grammar.compile g in
      let e = diag sev 0 in
      let emitted = P.(if first then emit e *> p else p <* emit e) in
      match P.parse p s with
      | exception P.Grammar_error _ -> true
      | plain ->
          let ran = first || Result.is_ok (P.parse_prefix p s) in
          let result = P.parse emitted s in
          let value, log = P.parse_partial emitted s in
          let has_error = sev = `Error && ran in
          Result.is_ok result = (Result.is_ok plain && not has_error)
          && (match result with
            | Ok _ -> log = [] || (sev = `Warning && value <> None)
            | Error ds -> ds = log)
          && List.exists (fun d -> d == e) log = ran
          && (sev = `Error || Result.is_ok result = Result.is_ok plain))

(* The log is ordered by where diagnostics point, and diagnostics that point
   to the same place stay in the order they were emitted. *)
let emission_order =
  Prop.test ~name:"log: ordered by start, equal starts in emission order"
    ~print:Print.(list (pair int bool))
    Gen.(list_size (Prop.range 0 12) (pair (int_bound 3) bool))
    (fun emitted ->
      let named =
        List.mapi
          (fun i (start, error) ->
            (Printf.sprintf "d%d" i, start, if error then `Error else `Warning))
          emitted
      in
      let p =
        List.fold_left
          (fun acc (name, start, severity) ->
            P.(
              acc
              *> emit
                   (D.make ~severity
                      ~span:(P.Span.make ~start ~stop:start)
                      name)))
          (P.return ()) named
      in
      let expected =
        List.map
          (fun (name, _, _) -> name)
          (List.stable_sort (fun (_, a, _) (_, b, _) -> compare a b) named)
      in
      List.map D.message (snd (P.parse_partial p "")) = expected)

(* Transactional diagnostics *)

(* Grammars over [a b c] whose every character logs a warning at its
   position, before matching. A parse that succeeds must have logged exactly
   the characters of its successful derivation, i.e. each position below the
   final one once, in order: whatever abandoned branches logged is gone. *)
type g =
  | Chr of char
  | Seq of g * g
  | Alt of g * g
  | Choice of g list
  | Many of g
  | Opt of g
  | Sep_by of g * g
  | Sep_end_by of g * g
  | Many_till of g * g
  | Look of g
  | Not of g
  | Recover of g  (** [`Any], syncing by skipping one token *)
  | Or_insert of char
  | Skip_to of g
  | Skip_past of g

let rec show = function
  | Chr c -> String.make 1 c
  | Seq (a, b) -> Printf.sprintf "(%s %s)" (show a) (show b)
  | Alt (a, b) -> Printf.sprintf "(%s | %s)" (show a) (show b)
  | Choice l -> "choice[" ^ String.concat "; " (List.map show l) ^ "]"
  | Many g -> show g ^ "*"
  | Opt g -> show g ^ "?"
  | Sep_by (s, g) -> Printf.sprintf "sep_by(%s,%s)" (show s) (show g)
  | Sep_end_by (s, g) -> Printf.sprintf "sep_end_by(%s,%s)" (show s) (show g)
  | Many_till (p, s) -> Printf.sprintf "many_till(%s,%s)" (show p) (show s)
  | Look g -> "&" ^ show g
  | Not g -> "!" ^ show g
  | Recover g -> "recover(" ^ show g ^ ")"
  | Or_insert c -> Printf.sprintf "or_insert(%c)" c
  | Skip_to g -> "skip_to(" ^ show g ^ ")"
  | Skip_past g -> "skip_past(" ^ show g ^ ")"

let ticking p =
  P.map
    (fun x ->
      Prop.tick ();
      x)
    p

let rec compile =
  let open P in
  function
  | Chr c ->
      let* pos = position in
      emit (diag `Warning pos) *> (char c >>| ignore)
  | Seq (a, b) -> compile a *> compile b
  | Alt (a, b) -> compile a <|> compile b
  | Choice l -> choice (List.map compile l)
  | Many g -> skip_many (ticking (compile g))
  | Opt g -> option () (compile g)
  | Sep_by (sep, g) ->
      sep_by ~sep:(compile sep) (ticking (compile g)) >>| ignore
  | Sep_end_by (sep, g) ->
      sep_end_by ~sep:(compile sep) (ticking (compile g)) >>| ignore
  | Many_till (p, stop) ->
      many_till (ticking (compile p)) (compile stop) >>| ignore
  | Look g -> lookahead (compile g)
  | Not g -> not_followed_by (compile g)
  | Recover g -> recover ~catch:`Any ~sync:any ~fallback:ignore (compile g)
  | Or_insert c ->
      let* pos = position in
      or_insert () (emit (diag `Warning pos) *> (char c >>| ignore))
  | Skip_to g -> skip_to (compile g)
  | Skip_past g -> skip_past (compile g)

let gen_g ~extended =
  let open Gen in
  let leaf = map (fun c -> Chr c) (oneof_list [ 'a'; 'b'; 'c' ]) in
  let node self n =
    let sub = self (n / 2) and next = self (n - 1) in
    oneof_weighted
      ([
         (3, leaf);
         (4, map2 (fun a b -> Seq (a, b)) sub sub);
         (4, map2 (fun a b -> Alt (a, b)) sub sub);
         (1, map (fun l -> Choice l) (list_size (int_bound 3) sub));
         (2, map (fun g -> Many g) next);
         (1, map (fun g -> Opt g) next);
         (2, map2 (fun a b -> Sep_by (a, b)) sub sub);
         (3, map2 (fun a b -> Sep_end_by (a, b)) sub sub);
         (2, map2 (fun a b -> Many_till (a, b)) sub sub);
         (1, map (fun g -> Look g) next);
         (1, map (fun g -> Not g) next);
       ]
      @
      if extended then
        [
          (2, map (fun g -> Recover g) next);
          (2, map (fun c -> Or_insert c) (oneof_list [ 'a'; 'b'; 'c' ]));
          (1, map (fun g -> Skip_to g) next);
          (1, map (fun g -> Skip_past g) next);
        ]
      else [])
  in
  sized_size (Prop.bound 8)
    (fix (fun self n -> if n <= 0 then leaf else node self n))

(* The log of a run, against a transactional model: a straightforward
   interpreter that threads the log through the grammar and drops it back
   wherever the contract says a branch is forgotten. Warnings are the [Chr]
   markers and errors are recoveries (found at an offset, deduped against the
   latest one) and the final failure. For the failure position it tracks the
   furthest live failure: [Done (pos, hint)], [Failed furthest]. *)
type entry = Warn of int | Err of int
type res = Done of int * int | Failed of int

exception Zero_width

let model_run g s =
  let n = String.length s in
  let record_err f log =
    match List.find_opt (function Err _ -> true | Warn _ -> false) log with
    | Some (Err g) when g = f -> log
    | _ -> Err f :: log
  in
  let rec go g pos log =
    match g with
    | Chr c ->
        let log = Warn pos :: log in
        if pos < n && s.[pos] = c then (Done (pos + 1, -1), log)
        else (Failed pos, log)
    | Seq (a, b) -> (
        match go a pos log with
        | (Failed _ as r), log -> (r, log)
        | Done (p1, h1), log -> (
            match go b p1 log with
            | Failed e, log -> (Failed (max e h1), log)
            | Done (p2, h2), log -> (Done (p2, max h1 h2), log)))
    | Alt (a, b) -> (
        match go a pos log with
        | (Done _ as r), log -> (r, log)
        | Failed ea, _ -> (
            match go b pos log with
            | Done (p, h), l -> (Done (p, max h ea), l)
            | Failed eb, l -> (Failed (max ea eb), l)))
    | Choice alternatives ->
        (* Folding [Alt]: the last alternative's failure keeps its log. *)
        let rec first errs = function
          | [] -> (Failed (List.fold_left max pos errs), log)
          | p :: rest -> (
              match go p pos log with
              | Done (q, h), l -> (Done (q, List.fold_left max h errs), l)
              | Failed e, l when rest = [] ->
                  (Failed (List.fold_left max e errs), l)
              | Failed e, _ -> first (e :: errs) rest)
        in
        first [] alternatives
    | Many g ->
        let rec loop pos log hint =
          match go g pos log with
          | Done (p, h), l ->
              if p = pos then raise Zero_width;
              loop p l (max hint h)
          | Failed e, _ -> (Done (pos, max hint e), log)
        in
        loop pos log (-1)
    | Opt g -> (
        match go g pos log with
        | (Done _ as r), l -> (r, l)
        | Failed e, _ -> (Done (pos, e), log))
    | Sep_by (sep, g) -> (
        match go g pos log with
        | Failed e, _ -> (Done (pos, e), log)
        | Done (p1, h1), log1 ->
            let rec loop pos log hint =
              match go sep pos log with
              | Failed e, _ -> (Done (pos, max hint e), log)
              | Done (ps, hs), l -> (
                  match go g ps l with
                  | Failed e, _ -> (Done (pos, max hint (max hs e)), log)
                  | Done (pg, hg), l2 ->
                      if pg = pos then raise Zero_width;
                      loop pg l2 (max hint (max hs hg)))
            in
            loop p1 log1 h1)
    | Sep_end_by (sep, g) -> (
        match go g pos log with
        | Failed e, _ -> (Done (pos, e), log)
        | Done (p1, h1), log1 ->
            let rec loop pos log hint =
              match go sep pos log with
              | Failed e, _ -> (Done (pos, max hint e), log)
              | Done (ps, hs), l -> (
                  let hint = max hint hs in
                  match go g ps l with
                  | Done (pg, hg), l2 ->
                      if pg = pos then raise Zero_width;
                      loop pg l2 (max hint hg)
                  | Failed e, _ -> (Done (ps, max hint e), l))
            in
            loop p1 log1 h1)
    | Many_till (g, stop) ->
        let rec loop pos log hint =
          match go stop pos log with
          | Done (ps, hs), l -> (Done (ps, max hint hs), l)
          | Failed es, _ -> (
              match go g pos log with
              | Done (pp, hp), l ->
                  if pp = pos then raise Zero_width;
                  loop pp l (max hint (max es hp))
              | Failed ep, l -> (Failed (max hint (max es ep)), l))
        in
        loop pos log (-1)
    | Look g -> (
        match go g pos log with
        | Done _, _ -> (Done (pos, -1), log)
        | (Failed _ as r), _ -> (r, log))
    | Not g -> (
        match go g pos log with
        | Failed _, _ -> (Done (pos, -1), log)
        | Done _, _ -> (Failed pos, log))
    | Recover g -> (
        match go g pos log with
        | (Done _ as r), l -> (r, l)
        | Failed e, l ->
            if e < n then (Done (e + 1, -1), record_err e l) else (Failed e, l))
    | Or_insert c ->
        if pos < n && s.[pos] = c then (Done (pos + 1, -1), Warn pos :: log)
        else (Done (pos, -1), record_err pos log)
    | Skip_to g -> skip ~past:false g pos log
    | Skip_past g -> skip ~past:true g pos log
  and skip ~past g pos log =
    let rec scan q =
      match go g q log with
      | Done (e, _), _ -> Done ((if past then e else q), -1)
      | Failed _, _ -> if q >= n then Done (q, -1) else scan (q + 1)
    in
    (scan pos, log)
  in
  match go g 0 [] with
  | Done (p, _), log -> (Some p, log)
  | Failed e, log -> (None, record_err e log)

(* What [parse_partial] reports: errors and warnings by where they point,
   stable among equal places. *)
let observed_log log = List.map (fun d -> (D.is_error d, start_of d)) log

let model_log log =
  let at = function Warn p -> (false, p) | Err p -> (true, p) in
  List.stable_sort (fun (_, a) (_, b) -> compare a b) (List.rev_map at log)

let transactional_log =
  Prop.test
    ~name:
      "diagnostics: the log is exactly that of the successful path, abandoned \
       branches and failed recoveries leave nothing"
    ~print:(fun (g, s) -> Printf.sprintf "%s on %S" (show g) s)
    (Gen.pair (gen_g ~extended:true) Parkour_test_support.Grammar.gen_input)
    (fun (g, s) ->
      let p = P.(compile g *> position <* skip_many any) in
      let engine =
        try Some (P.parse_partial p s) with P.Grammar_error _ -> None
      in
      let model = try Some (model_run g s) with Zero_width -> None in
      match (engine, model) with
      | None, None -> true
      | Some (value, log), Some (stop, entries) ->
          value = stop && observed_log log = model_log entries
      | _ -> false)

(* [quoted]'s escape handling turns a failing [escape] into its own error: what
   [escape] logged before failing must go. *)
let quoted_escape =
  Prop.test ~name:"a failed escape sequence leaves only its own error"
    ~print:Print.char
    (Gen.oneof_list [ 'q'; 'x'; 'z' ])
    (fun c ->
      let escape = P.(emit (diag `Warning 0) *> string "\\n") in
      let p =
        P.(
          recover ~catch:`Any ~sync:skip_line
            ~fallback:(fun _ -> "")
            (quoted ~escape '"'))
      in
      let _, log = P.parse_partial p (Printf.sprintf "\"\\%c\"\n" c) in
      List.map D.message log = [ "invalid escape sequence" ])

(* Recorded context *)

(* [levels] nested contexts, each consuming an ['x']; the leaf is ['y'], or a
   variant with an abandoned alternative or optional prefix. The recovering
   variant wraps level [at] in a [recover] that skips the rest. *)
let nested ~levels ~at ~outer ~leaf ~recovering =
  let open P in
  let leaf =
    match leaf with
    | 0 -> char 'y'
    | 1 -> char 'y' <|> char 'w'
    | _ -> optional (char 'q') *> char 'y'
  in
  let rec level i =
    if i > levels then leaf >>| ignore
    else
      let p =
        context (Printf.sprintf "level%d" i) (char 'x' *> level (i + 1))
      in
      if recovering && i = at then
        recover ~catch:`Any ~sync:(skip_to eof) ~fallback:ignore p
      else p
  in
  if outer then context "outer" (level 1) else level 1

let recorded_context =
  Prop.test ~name:"a recorded diagnostic has the context of the unrecovered one"
    ~print:(fun (levels, at, xs, tail, outer, leaf) ->
      Printf.sprintf "levels=%d recover at %d on %dx then %S%s, leaf %d" levels
        at xs tail
        (if outer then " in a context" else "")
        leaf)
    Gen.(
      let* levels = int_range 1 4 in
      let* at = int_range 1 levels in
      let* xs = int_bound (levels + 1) in
      let* tail = oneof_list [ ""; "z"; "xz"; "q"; "w" ] in
      let* outer = bool in
      let* leaf = int_bound 2 in
      return (levels, at, xs, tail, outer, leaf))
    (fun (levels, at, xs, tail, outer, leaf) ->
      let input = String.make xs 'x' ^ tail in
      match
        ( P.parse (nested ~levels ~at ~outer ~leaf ~recovering:false) input,
          P.parse (nested ~levels ~at ~outer ~leaf ~recovering:true) input )
      with
      | Ok (), Ok () -> true
      | Error [ plain ], Error (recovered :: _) ->
          D.context plain = D.context recovered
          && D.message plain = D.message recovered
          && D.span plain = D.span recovered
      | _ -> false)

(* skip_to / skip_past *)

let naive ~past ~nested s =
  let n = String.length s in
  let result = ref None and depth = ref 0 and i = ref 0 in
  while !result = None do
    (if !depth = 0 && !i < n && s.[!i] = ';' then
       result := Some (if past then !i + 1 else !i)
     else if !i >= n then result := Some n
     else
       match nested with
       | Some (o, _) when s.[!i] = o -> incr depth
       | Some (_, c) when s.[!i] = c && !depth = 0 -> result := Some !i
       | Some (_, c) when s.[!i] = c -> decr depth
       | _ -> ());
    incr i
  done;
  Option.get !result

let skipping =
  Prop.test ~name:"skip_to and skip_past agree with a bracket scanner"
    ~print:(fun (s, past, nested) ->
      Printf.sprintf "%S past=%b nested=%s" s past
        (match nested with
        | None -> "-"
        | Some (o, c) -> Printf.sprintf "%c%c" o c))
    Gen.(
      triple
        (string_size
           ~gen:(oneof_list [ '('; ')'; '{'; '}'; ';'; 'a' ])
           (Prop.bound 12))
        bool
        (oneof_list [ None; Some ('(', ')'); Some ('{', '}') ]))
    (fun (s, past, nested) ->
      let nested' = Option.map (fun (o, c) -> (P.char o, P.char c)) nested in
      let skip = if past then P.skip_past else P.skip_to in
      P.parse_prefix (skip ?nested:nested' (P.char ';')) s
      = Ok ((), naive ~past ~nested s))

(* max_errors and dedupe *)

let limit =
  Prop.test ~name:"max_errors k with n errors logs min(n, k) and still finishes"
    ~print:(fun (n, k) -> Printf.sprintf "n=%d k=%d" n k)
    Gen.(pair (int_bound 8) (int_range 1 10))
    (fun (n, k) ->
      let line = P.(char 'q' *> char '=' *> cut *> char 'v' *> newline) in
      let p = P.(many (recover ~sync:skip_line ~fallback:(fun _ -> ()) line)) in
      let input = String.concat "" (List.init n (fun _ -> "q=\n")) in
      let value, log = P.parse_partial ~max_errors:k p input in
      List.length (errors log) = min n k && value = Some (List.init n ignore))

(* Tokens whose spans are chosen freely (gaps, any order), to tell offsets
   from token indices, and a token's span from where a failure is found. *)
module Chars = P.Tokens (struct
  type t = char

  let equal = Char.equal
  let show = String.make 1
end)

(* The span the fallback receives runs from where the recovered parser started
   (not from the failure) to where [sync] stopped, whatever lies between the
   tokens. *)
let fallback_span =
  Prop.test
    ~name:
      "recover: the fallback gets the span from the parser's start to where \
       sync stopped"
    ~print:(fun (before, junk, gaps, past) ->
      Printf.sprintf "%d before, junk %S, gaps %s, %s" before junk
        (Print.(list int) gaps)
        (if past then "skip_past" else "skip_to"))
    Gen.(
      let* before = int_bound 3 in
      let* junk =
        string_size ~gen:(oneof_list [ '!'; '?'; 'x' ]) (int_range 1 4)
      in
      let* gaps =
        list_size (return (before + String.length junk + 2)) (int_bound 3)
      in
      let* past = bool in
      return (before, junk, gaps, past))
    (fun (before, junk, gaps, past) ->
      (* [p* a junk ;]: the error is at the first junk character. *)
      let chars = String.make before 'p' ^ "a" ^ junk ^ ";" in
      let last = String.length chars - 1 in
      let stop_index = if past then last else last - 1 in
      let body_start = before in
      (* Text: consecutive characters *)
      let text_parser sync =
        P.(
          skip_many (char 'p')
          *> recover ~sync ~fallback:Fun.id
               (char 'a' *> cut *> char 'b' >>| fun _ -> Span.point 0)
          <* skip_many any)
      in
      let text_sync =
        if past then P.skip_past (P.char ';') else P.skip_to (P.char ';')
      in
      (* Tokens: the same, with gaps between the spans *)
      let starts =
        let at = ref 0 in
        List.map
          (fun gap ->
            at := !at + gap;
            let s = !at in
            incr at;
            s)
          gaps
      in
      let tokens =
        Array.of_list
          (List.mapi
             (fun i s -> (chars.[i], P.Span.make ~start:s ~stop:(s + 1)))
             starts)
      in
      let input = Chars.input ~source:(String.make 40 ' ') tokens in
      let token_parser =
        Chars.(
          skip_many (token 'p')
          *> recover
               ~sync:
                 (if past then skip_past (token ';') else skip_to (token ';'))
               ~fallback:Fun.id
               (token 'a' *> cut *> token 'b' >>| fun _ -> P.Span.point 0)
          <* skip_many any)
      in
      let at i = List.nth starts i in
      fst (P.parse_partial (text_parser text_sync) chars)
      = Some (P.Span.make ~start:body_start ~stop:(stop_index + 1))
      && fst (Chars.parse_partial token_parser input)
         = Some (P.Span.make ~start:(at body_start) ~stop:(at stop_index + 1)))

(* Recoveries are deduped against the latest recovery error, by the offset
   where the failure was found; where the diagnostic points (for a missing
   token, just after the previous one) has nothing to do with it. Offsets here
   are free, so they need not grow. Steps: a word and some trivia tokens after
   it, a missing ';' inserted, a bad token recovered from, and emitted
   diagnostics anywhere. *)
type step =
  | Word of int  (** trivia tokens after the word *)
  | Insert
  | Skip
  | Emit_error of int
  | Emit_warning of int

let show_step = function
  | Word k -> Printf.sprintf "word+%d" k
  | Insert -> "insert"
  | Skip -> "skip"
  | Emit_error i -> Printf.sprintf "error@%d" i
  | Emit_warning i -> Printf.sprintf "warning@%d" i

let eof_offset = 100

let dedupe_by_found_offset =
  Prop.test
    ~name:
      "recovery errors are deduped by the offset where the failure was found, \
       against the latest one; emitted diagnostics never"
    ~print:(fun (steps, offsets) ->
      Printf.sprintf "%s with token offsets %s"
        (String.concat "; " (List.map show_step steps))
        (Print.(list int) offsets))
    Gen.(
      let* rest =
        list_size (Prop.range 1 10)
          (oneof_weighted
             [
               (3, map (fun k -> Word k) (int_bound 2));
               (4, return Insert);
               (2, return Skip);
               (1, map (fun i -> Emit_error i) (int_bound 5));
               (1, map (fun i -> Emit_warning i) (int_bound 5));
             ])
      in
      let steps = Word 1 :: rest in
      let tokens =
        List.fold_left
          (fun n -> function Word k -> n + 1 + k | Skip -> n + 1 | _ -> n)
          0 steps
      in
      (* Spans start at small offsets, repeated and out of order. *)
      let* offsets = list_size (return tokens) (int_bound 6) in
      return (steps, offsets))
    (fun (steps, offsets) ->
      let tokens =
        List.concat_map
          (function
            | Word k -> 'w' :: List.init k (fun _ -> 's')
            | Skip -> [ 'j' ]
            | _ -> [])
          steps
      in
      let input =
        Chars.input
          ~source:(String.make eof_offset ' ')
          (Array.of_list
             (List.map2
                (fun c o -> (c, P.Span.make ~start:o ~stop:(o + 1)))
                tokens offsets))
      in
      let step = function
        | Word _ -> Chars.(token 'w' *> trivia (skip_many (token 's')))
        | Insert -> Chars.(or_insert () (token ';' >>| ignore))
        | Skip ->
            Chars.(
              recover ~catch:`Any ~sync:any ~fallback:ignore
                (token 'z' >>| ignore))
        | Emit_error i -> Chars.emit (diag `Error i)
        | Emit_warning i -> Chars.emit (diag `Warning i)
      in
      let p =
        List.fold_left
          (fun acc s -> Chars.(acc *> step s))
          (Chars.return ()) steps
      in
      (* The model *)
      let off = Array.of_list offsets in
      let count = Array.length off in
      let start_at i = if i < count then off.(i) else eof_offset in
      let log = ref [] (* newest first: (is_error, shown, found option) *)
      and pos = ref 0
      and trivia = ref None in
      let recovery ~shown ~found =
        let latest = List.find_map (fun (_, _, f) -> f) !log in
        if latest <> Some found then log := (true, shown, Some found) :: !log
      in
      List.iter
        (function
          | Word k ->
              pos := !pos + 1;
              if k > 0 then trivia := Some (!pos, !pos + k);
              pos := !pos + k
          | Insert ->
              let shown =
                match !trivia with
                | Some (a, b) when b = !pos -> start_at a
                | _ ->
                    if !pos > 0 then start_at (!pos - 1) + 1 else start_at !pos
              in
              recovery ~shown ~found:(start_at !pos)
          | Skip ->
              recovery ~shown:(start_at !pos) ~found:(start_at !pos);
              pos := !pos + 1
          | Emit_error i -> log := (true, i, None) :: !log
          | Emit_warning i -> log := (false, i, None) :: !log)
        steps;
      let expected =
        List.stable_sort
          (fun (_, a) (_, b) -> compare a b)
          (List.rev_map (fun (e, shown, _) -> (e, shown)) !log)
      in
      let value, got = Chars.parse_partial p input in
      value <> None && observed_log got = expected)

(* or_insert *)

let inserting =
  Prop.test ~name:"or_insert logs each missing token once and continues"
    ~print:Print.(list bool)
    Gen.(list_size (Prop.bound 8) bool)
    (fun present ->
      let input =
        String.concat "" (List.map (fun b -> if b then "a;" else "a") present)
      in
      let item = P.(char 'a' *> or_insert ';' (char ';')) in
      let missing_at =
        let pos = ref 0 in
        List.filter_map
          (fun b ->
            pos := !pos + if b then 2 else 1;
            if b then None else Some !pos)
          present
      in
      let value, log = P.parse_partial (P.many item) input in
      value = Some (List.map (fun _ -> ';') present)
      && List.map start_of log = missing_at
      && Result.is_ok (P.parse (P.many item) input) = (missing_at = []))

(* Insertion points: the trivia range is part of the path taken. The missing
   ';' is always reported just after the 'a', before the whitespace. *)

let gen_ws =
  Gen.(string_size ~gen:(oneof_list [ ' '; '\n'; '\t' ]) (int_bound 3))

let insertion_start p input =
  match P.parse_partial P.(p <* skip_many any) input with
  | Some (), [ d ] -> Some (start_of d)
  | _ -> None

(* A parser whose [on]th run logs a warning, consumes trivia and then fails,
   and that does nothing on the others. *)
let fails_after_trivia ~on =
  let calls = ref 0 in
  P.(
    let* () = return () in
    incr calls;
    if !calls = on then
      let* pos = position in
      emit (diag `Warning pos) *> whitespace *> fail "no"
    else return ())

(* Constructs that forget a branch (a failure, or a lookahead) after it
   consumed trivia, and where else they report something: the offset where
   the input after the 'x' starts. *)
let forgetting_constructs =
  [
    ("alt", (fun () -> P.(fails_after_trivia ~on:1 <|> return ())), fun _ -> []);
    ( "choice",
      (fun () -> P.(choice [ fails_after_trivia ~on:1; return () ])),
      fun _ -> [] );
    ( "skip_many",
      (fun () -> P.(skip_many (fails_after_trivia ~on:1))),
      fun _ -> [] );
    ( "sep_end_by, separator",
      (fun () ->
        P.(sep_end_by ~sep:(fails_after_trivia ~on:1) (return ()) >>| ignore)),
      fun _ -> [] );
    ( "sep_end_by, item",
      (fun () ->
        P.(sep_end_by ~sep:(return ()) (fails_after_trivia ~on:2) >>| ignore)),
      fun _ -> [] );
    ( "many_till",
      (fun () ->
        P.(
          many_till (skip_while is_space) (fails_after_trivia ~on:1) >>| ignore)),
      fun _ -> [] );
    ( "lookahead",
      (fun () ->
        P.(
          lookahead
            (let* pos = position in
             emit (diag `Warning pos) *> whitespace))),
      fun _ -> [] );
    ( "not_followed_by",
      (fun () -> P.not_followed_by (fails_after_trivia ~on:1)),
      fun _ -> [] );
    ( "skip_to",
      (fun () -> P.(skip_to (fails_after_trivia ~on:1) >>| ignore)),
      fun _ -> [] );
    (* A closer that fails where it starts: the missing token is inserted *)
    ( "between ~or_insert",
      (fun () ->
        P.(
          between ~or_insert:true (return ())
            (let* pos = position in
             emit (diag `Warning pos) *> (char ')' >>| ignore))
            (return ())
          >>| ignore)),
      fun after_x -> [ after_x ] );
    (* A recovery that cannot synchronise (its [sync] consumes trivia, then
       fails) is forgotten by the recovery around it, which records the
       original failure where it was found. *)
    ( "recover, failed synchronisation",
      (fun () ->
        P.(
          recover ~catch:`Any ~sync:(skip_to eof) ~fallback:ignore
            (recover ~catch:`Any
               ~sync:
                 (let* pos = position in
                  emit (diag `Warning pos) *> whitespace *> fail "stuck")
               ~fallback:ignore
               (char '!' >>| ignore))
          >>| ignore)),
      fun after_x -> [ after_x ] );
  ]

let forgotten_branches_leave_nothing =
  Prop.test
    ~name:
      "forgotten branches (lookahead, alternatives, loops, skipping, failed \
       recovery) leave no diagnostics and no trivia behind"
    ~print:(fun (ws1, ws2) -> Printf.sprintf "a%Sx%S" ws1 ws2)
    Gen.(
      pair gen_ws
        (string_size ~gen:(oneof_list [ ' '; '\n'; '\t' ]) (int_range 1 3)))
    (fun (ws1, ws2) ->
      let input = "a" ^ ws1 ^ "x" ^ ws2 in
      (* A construct forgets trivia that ends where the input does; the
         parser then reads that stretch as part of 'x' (unmarked), so the
         missing ';' goes at the end of the input, which no trivia precedes. *)
      List.for_all
        (fun (_, construct, elsewhere) ->
          let p =
            P.(
              char 'a' *> whitespace *> char 'x' *> construct ()
              *> skip_while is_space
              *> or_insert () (char ';' >>| ignore))
          in
          match P.parse_partial P.(p <* skip_many any) input with
          | Some (), log ->
              List.map start_of log
              = List.sort compare
                  (String.length input :: elsewhere (2 + String.length ws1))
          | _ -> false)
        forgetting_constructs)

let insertion_unchanged_by_splitting_trivia =
  Prop.test ~name:"adjacent trivia parsers give the insertion point of one"
    ~print:(fun chunks -> Print.(list string) chunks)
    Gen.(
      list_size (int_range 1 4)
        (string_size ~gen:(oneof_list [ ' '; '\n'; '\t' ]) (int_range 1 3)))
    (fun chunks ->
      let input = "a" ^ String.concat "" chunks ^ "x" in
      let ws = List.map (fun c -> P.(trivia (string c))) chunks in
      let p =
        P.(
          char 'a'
          *> List.fold_right (fun w acc -> w *> acc) ws (return ())
          *> or_insert () (char ';' >>| ignore))
      in
      insertion_start p input = Some 1)

let hidden_does_not_move_insertion_point =
  Prop.test ~name:"hidden p and p give the same insertion point"
    ~print:(fun (ws, trivia) -> Printf.sprintf "a%S, trivia=%b" ws trivia)
    Gen.(pair gen_ws bool)
    (fun (ws, is_trivia) ->
      let input = "a" ^ ws ^ "x" in
      let skip = P.skip_while ~label:"space" P.is_space in
      let skip = if is_trivia then P.trivia skip else skip in
      let at p =
        insertion_start
          P.(char 'a' *> p *> or_insert () (char ';' >>| ignore))
          input
      in
      (* Unmarked whitespace is part of the previous token as far as the
         insertion point goes. *)
      let expected = if is_trivia then 1 else 1 + String.length ws in
      at skip = Some expected && at (P.hidden skip) = Some expected)

(* Rules of the recording log *)

let committed_catch_only =
  Prop.test ~name:"recover leaves uncommitted failures to alternatives"
    ~print:(fun (a, b, s) ->
      Printf.sprintf "%s | %s on %S"
        (Parkour_test_support.Grammar.to_string a)
        (Parkour_test_support.Grammar.to_string b)
        s)
    Gen.(
      triple
        (Parkour_test_support.Grammar.gen ~cuts:false)
        (Parkour_test_support.Grammar.gen ~cuts:false)
        Parkour_test_support.Grammar.gen_input)
    (fun (a, b, s) ->
      let module G = Parkour_test_support.Grammar in
      let run p =
        match P.parse_partial p s with
        | exception P.Grammar_error _ -> None
        | v, log -> Some (v, List.map (fun d -> (D.message d, D.span d)) log)
      in
      run
        P.(
          recover ~sync:(skip_to eof) ~fallback:(fun _ -> "") (G.compile a)
          <|> G.compile b)
      = run P.(G.compile a <|> G.compile b))

let abandoned_errors_do_not_count =
  Prop.test ~name:"errors of abandoned branches are neither shown nor counted"
    ~print:(fun (m, k) -> Printf.sprintf "%d insertions, max_errors=%d" m k)
    Gen.(pair (int_range 1 3) (int_range 1 3))
    (fun (m, k) ->
      let insert = P.(or_insert ';' (char ';')) in
      let branch =
        List.fold_left
          (fun acc _ -> P.(insert *> acc))
          (P.char 'z') (List.init m Fun.id)
      in
      P.parse_partial ~max_errors:k P.(branch <|> char 'a') "a" = (Some 'a', []))

let final_failure_not_hidden =
  Prop.test ~name:"an emitted error does not hide the final failure"
    ~print:string_of_int (Gen.int_bound 3) (fun start ->
      let value, log =
        P.parse_partial P.(emit (diag `Error start) *> char 'x') "abc"
      in
      value = None && List.length (errors log) = 2)

let recovered_error_not_hidden =
  Prop.test ~name:"what sync logs does not hide the recovered error"
    ~print:string_of_bool Gen.bool (fun use_emit ->
      let sync =
        if use_emit then P.(emit (diag `Error 1) *> skip_line)
        else P.(or_insert '!' (char '!') *> skip_line)
      in
      let p =
        P.(
          recover ~catch:`Any ~sync
            ~fallback:(fun _ -> ())
            (char 'a' *> cut *> char 'b' >>| ignore))
      in
      let value, log = P.parse_partial p "ax\n" in
      value = Some ()
      && List.exists (fun d -> D.message d = "expected 'b', found 'x'") log
      && List.length log = if use_emit then 2 else 1)

(* The inner recovery cannot synchronise, so its failure goes on to the outer
   one, which sees a further error (from the hint of [optional]) and records
   that one only. *)
let failed_recovery_records_nothing =
  Prop.once ~name:"a recovery that cannot synchronise records nothing"
    (fun () ->
      let inner =
        P.(
          recover ~catch:`Any ~sync:(fail "stuck") ~fallback:ignore
            (char 'z' >>| ignore))
      in
      let outer =
        P.(
          recover ~catch:`Any ~sync:(skip_to eof) ~fallback:ignore
            (optional (char 'a' *> char 'b' *> char 'c') *> inner))
      in
      let _, log = P.parse_partial outer "abd" in
      List.map start_of log = [ 2 ])

let () =
  Prop.run "recovery"
    [
      corrupt_one;
      terminates;
      emit_contract;
      emission_order;
      transactional_log;
      quoted_escape;
      committed_catch_only;
      abandoned_errors_do_not_count;
      final_failure_not_hidden;
      recovered_error_not_hidden;
      failed_recovery_records_nothing;
      recorded_context;
      forgotten_branches_leave_nothing;
      insertion_unchanged_by_splitting_trivia;
      hidden_does_not_move_insertion_point;
      skipping;
      limit;
      fallback_span;
      dedupe_by_found_offset;
      inserting;
    ]
