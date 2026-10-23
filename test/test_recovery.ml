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
    list_size (int_range 1 6)
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
      (int_bound 40))

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

let rec compile =
  let open P in
  function
  | Chr c ->
      let* pos = position in
      emit (diag `Warning pos) *> (char c >>| ignore)
  | Seq (a, b) -> compile a *> compile b
  | Alt (a, b) -> compile a <|> compile b
  | Choice l -> choice (List.map compile l)
  | Many g -> skip_many (compile g)
  | Opt g -> option () (compile g)
  | Sep_by (sep, g) -> sep_by ~sep:(compile sep) (compile g) >>| ignore
  | Sep_end_by (sep, g) -> sep_end_by ~sep:(compile sep) (compile g) >>| ignore
  | Many_till (p, stop) -> many_till (compile p) (compile stop) >>| ignore
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
  sized_size (int_bound 8)
    (fix (fun self n -> if n <= 0 then leaf else node self n))

(* With recovery in the grammar, skipped input is unmarked, so only check that
   the warnings of the successful derivation come in increasing positions,
   each once, below the end: a leaked marker is re-read, so it repeats or
   lies out of order. *)
let abandoned_branches_leave_nothing ~extended =
  Prop.test
    ~name:
      ("diagnostics of abandoned branches never appear"
      ^ if extended then " (with recovery)" else "")
    ~print:(fun (g, s) -> Printf.sprintf "%s on %S" (show g) s)
    (Gen.pair (gen_g ~extended) Parkour_test_support.Grammar.gen_input)
    (fun (g, s) ->
      let p = P.(compile g *> position <* skip_many any) in
      match P.parse_partial p s with
      | exception P.Grammar_error _ -> true
      | None, _ -> true
      | Some stop, log ->
          let marks =
            List.map start_of (List.filter (fun d -> not (D.is_error d)) log)
          in
          if extended then
            List.for_all (fun m -> m < stop) marks
            && List.sort_uniq compare marks = marks
          else marks = List.init stop Fun.id)

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
           (int_bound 12))
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

type op = Insert | Emit_error of int | Emit_warning of int

let dedupe =
  Prop.test
    ~name:"only recovery errors are deduped, against the latest recovery error"
    ~print:
      Print.(
        list (function
          | Insert -> "insert"
          | Emit_error i -> "error " ^ string_of_int i
          | Emit_warning i -> "warning " ^ string_of_int i))
    Gen.(
      list_size (int_bound 10)
        (oneof
           [
             return Insert;
             map (fun i -> Emit_error i) (int_bound 3);
             map (fun i -> Emit_warning i) (int_bound 3);
           ]))
    (fun ops ->
      let step = function
        | Insert -> P.or_insert () P.(char 'x' >>| ignore)
        | Emit_error i -> P.emit (diag `Error i)
        | Emit_warning i -> P.emit (diag `Warning i)
      in
      let p =
        List.fold_left (fun acc op -> P.(acc *> step op)) (P.return ()) ops
      in
      (* All insertions fail at 0, so they are one cascade. *)
      let emitted = List.length (List.filter (fun o -> o <> Insert) ops) in
      let _, log = P.parse_partial p "" in
      List.length log = emitted + if List.mem Insert ops then 1 else 0)

(* The cascade rule is about where the failure was found, not where the
   diagnostic points: a missing token (shown at the end of the previous token)
   followed by an error at the same next token is one error. *)
let dedupe_insertion_point =
  Prop.test ~name:"an insertion and an error at the same found token are one"
    ~print:String.escaped
    Gen.(string_size ~gen:(oneof_list [ ' '; '\n' ]) (int_bound 4))
    (fun trivia ->
      let module L = (val P.Lexeme.make ()) in
      let p =
        P.(
          L.symbol "a"
          *> or_insert () (map ignore (L.symbol ";"))
          *> recover ~catch:`Any ~sync:(skip_to eof)
               ~fallback:(fun _ -> ())
               (map ignore (L.symbol "b")))
      in
      match P.parse_partial p ("a" ^ trivia ^ "#") with
      | _, [ d ] -> (D.span d).start = 1 && D.message d <> ""
      | _ -> false)

(* or_insert *)

let inserting =
  Prop.test ~name:"or_insert logs each missing token once and continues"
    ~print:Print.(list bool)
    Gen.(list_size (int_bound 8) bool)
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

(* Branches that consume trivia (before and after an 'x') and are then
   forgotten. *)
let abandoned_trivia = function
  | 0 -> P.return ()
  | 1 -> P.(lookahead (any_char *> whitespace))
  | 2 -> P.(optional (any_char *> whitespace *> char '!') >>| ignore)
  | _ -> P.(any_char *> whitespace *> char '!' >>| ignore <|> return ())

let insertion_unchanged_by_forgotten_trivia =
  Prop.test
    ~name:
      "the insertion point ignores trivia of lookaheads and abandoned branches"
    ~print:(fun (ws1, ws2, kind) ->
      Printf.sprintf "a%Sx%S, branch %d" ws1 ws2 kind)
    Gen.(triple gen_ws gen_ws (int_bound 3))
    (fun (ws1, ws2, kind) ->
      let input = "a" ^ ws1 ^ "x" ^ ws2 in
      let p =
        P.(
          char 'a' *> whitespace *> abandoned_trivia kind
          *> or_insert () (char ';' >>| ignore))
      in
      insertion_start p input = Some 1)

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
  Prop.test ~count:1 ~name:"a recovery that cannot synchronise records nothing"
    Gen.unit (fun () ->
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
      abandoned_branches_leave_nothing ~extended:false;
      abandoned_branches_leave_nothing ~extended:true;
      quoted_escape;
      committed_catch_only;
      abandoned_errors_do_not_count;
      final_failure_not_hidden;
      recovered_error_not_hidden;
      failed_recovery_records_nothing;
      recorded_context;
      insertion_unchanged_by_forgotten_trivia;
      insertion_unchanged_by_splitting_trivia;
      hidden_does_not_move_insertion_point;
      skipping;
      limit;
      dedupe;
      dedupe_insertion_point;
      inserting;
    ]
