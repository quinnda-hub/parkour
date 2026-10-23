module P = Parkour
module Prop = Parkour_test_support.Prop
module G = Parkour_test_support.Grammar
open QCheck2

let print_gi (g, s) = Printf.sprintf "%s on %S" (G.to_string g) s
let gen_gi ~cuts = Gen.pair (G.gen ~cuts) G.gen_input

(* Observations *)

type 'a obs = Parsed of 'a * int | Failed of (string * int * int) list | Gerr

let obs_prefix p s =
  match P.parse_prefix p s with
  | Ok (v, pos) -> Parsed (v, pos)
  | Error ds ->
      Failed
        (List.map
           (fun d ->
             let sp = P.Diagnostic.span d in
             (P.Diagnostic.message d, sp.start, sp.stop))
           ds)
  | exception P.Grammar_error _ -> Gerr

let obs_full p s = obs_prefix (P.map (fun v -> v) P.(p <* eof)) s
let obs2 p s = (obs_prefix p s, obs_full p s)

(* a. oracle agreement *)

type outcome = O_ok of string * int | O_err | O_gerr

let engine_outcome ~full p s =
  match
    if full then Result.map (fun v -> (v, 0)) (P.parse p s)
    else P.parse_prefix p s
  with
  | Ok (v, pos) -> O_ok (v, pos)
  | Error _ -> O_err
  | exception P.Grammar_error _ -> O_gerr

let oracle_outcome ~full g s =
  match if full then G.Oracle.run g s else G.Oracle.run_prefix g s with
  | G.Oracle.Ok (v, pos) -> O_ok (v, if full then 0 else pos)
  | Error _ -> O_err
  | exception P.Grammar_error _ -> O_gerr

let outcomes_agree =
  Prop.test ~name:"oracle: outcomes agree" ~print:print_gi (gen_gi ~cuts:true)
    (fun (g, s) ->
      let p = G.compile g in
      List.for_all
        (fun full -> engine_outcome ~full p s = oracle_outcome ~full g s)
        [ false; true ])

(* b. oracle error agreement *)

let errors_agree ~cuts =
  Prop.test
    ~name:
      (if cuts then "oracle: errors agree (with cuts)"
       else "oracle: errors agree (cut-free)")
    ~print:print_gi (gen_gi ~cuts)
    (fun (g, s) ->
      match P.parse (G.compile g) s with
      | exception P.Grammar_error _ -> true
      | Ok _ -> true
      | Error [ d ] -> (
          match G.Oracle.run g s with
          | exception P.Grammar_error _ -> false
          | Ok _ -> false
          | Error f -> (
              let sp = P.Diagnostic.span d in
              sp.start = f.pos
              &&
              match (f.custom, f.unexpected) with
              | Some m, _ -> P.Diagnostic.message d = m
              | None, Some stop when stop > f.pos ->
                  P.Diagnostic.message d
                  = "unexpected "
                    ^ Parkour_test_support.Quote.range s f.pos stop
                  && sp.stop = stop
                  && P.Diagnostic.expected d = []
              | None, _ ->
                  P.Diagnostic.found d <> None
                  && P.Diagnostic.expected d = f.expected))
      | Error _ -> false)

(* c. laws *)

let gen4 =
  let g = G.gen ~cuts:true in
  Gen.(pair (tup4 g g g g) (pair G.gen_input (oneof_list [ ""; "a"; "bc" ])))

let print4 ((m, p, q, r), (s, x)) =
  Printf.sprintf "m=%s p=%s q=%s r=%s input=%S x=%S" (G.to_string m)
    (G.to_string p) (G.to_string q) (G.to_string r) s x

let law name f =
  Prop.test ~name ~print:print4 gen4 (fun ((m, p, q, r), (s, x)) ->
      let m = G.compile m and p = G.compile p and q = G.compile q in
      let r = G.compile r in
      f m p q r x s)

let cont_f p q v = if v = "" then p else P.map (fun s -> v ^ s) q

let cont_g q r v =
  if String.length v mod 2 = 0 then r else P.map (fun s -> s ^ v) q

let law_tests =
  [
    law "law: bind left identity" (fun _ p q _ x s ->
        let f = cont_f p q in
        obs2 (P.bind (P.return x) f) s = obs2 (f x) s);
    law "law: bind right identity" (fun m _ _ _ _ s ->
        obs2 (P.bind m P.return) s = obs2 m s);
    law "law: bind associativity" (fun m p q r _ s ->
        let f = cont_f p q and g = cont_g q r in
        obs2 (P.bind (P.bind m f) g) s
        = obs2 (P.bind m (fun v -> P.bind (f v) g)) s);
    law "law: <|> associativity" (fun m p q _ _ s ->
        P.(obs2 (m <|> p <|> q) s = obs2 (m <|> (p <|> q)) s));
    law "law: return x <|> p = return x" (fun _ p _ _ x s ->
        obs2 P.(return x <|> p) s = obs2 (P.return x) s);
  ]

(* d. cuts *)

(* [attempt], [lookahead] and [not_followed_by] absorb a committed failure that
   an enclosing alternative or repetition would otherwise propagate, and
   [not_followed_by] turns an inner failure into success. A cut inside them can
   therefore change which branches run even when the whole parse succeeds, e.g.
   [Many (Attempt (Alt (Seq (Cut, Char 'a'), Eof)))] on "". The property is
   stated for grammars with no cut under any of the three. *)
(* Whether a cut ran matters: the rule says nothing about runs that never
   committed. [collect] reports how often the useful case is generated. *)
let cut_case (g, s) =
  let cuts = ref 0 in
  match P.parse (G.compile ~on_cut:(fun () -> incr cuts) g) s with
  | exception P.Grammar_error _ -> `Grammar_error
  | Error _ -> `Error
  | Ok v -> (
      let same =
        match P.parse (G.compile (G.erase_cuts g)) s with
        | Ok v' -> v = v'
        | Error _ | (exception P.Grammar_error _) -> false
      in
      match (!cuts > 0, same) with
      | true, true -> `Committed_same
      | false, true -> `Uncommitted_same
      | _, false -> `Differs)

let cut_class c =
  match cut_case c with
  | `Grammar_error -> "Grammar_error"
  | `Error -> "failure"
  | `Committed_same -> "success, a cut ran"
  | `Uncommitted_same -> "success, no cut ran"
  | `Differs -> "DIFFERS"

let cut_never_changes_success =
  Prop.test ~name:"cut never changes a successful result" ~print:print_gi
    ~collect:cut_class
    Gen.(
      map
        (fun (g, s) -> (G.erase_absorbed_cuts g, s))
        (G.gen_matching ~cuts:true))
    (fun c -> cut_case c <> `Differs)

(* e. stack safety *)

let big = 1_000_000

(* Angstrom #221: [count] was not tail-recursive; see [count big a] below. *)
let stack_safety =
  Prop.once ~name:"stack safety on 10^6 items" (fun () ->
      let a = P.char 'a' in
      let as_ = String.make big 'a' in
      let csv = String.concat "," (List.init big (fun _ -> "a")) in
      let ok_len n = function Ok l -> List.length l = n | Error _ -> false in
      ok_len big (P.parse (P.many a) as_)
      && ok_len big (P.parse (P.many1 a) as_)
      && ok_len big (P.parse (P.sep_by ~sep:(P.char ',') a) csv)
      && ok_len big (P.parse (P.sep_end_by ~sep:(P.char ',') a) (csv ^ ","))
      && ok_len big (P.parse (P.count big a) as_)
      && P.parse (P.skip_many a) as_ = Ok ()
      && P.parse (P.fold_many (fun n _ -> n + 1) 0 a) as_ = Ok big
      && ok_len big (P.parse (P.many_till a (P.char '!')) (as_ ^ "!")))

(* [sep_end_by1] against a model: [a (, a)* ,?] as far as it matches. *)
let sep_end_by1_model s =
  let n = String.length s in
  let at i c = i < n && s.[i] = c in
  if not (at 0 'a') then None
  else
    let rec go count i =
      if at i ',' && at (i + 1) 'a' then go (count + 1) (i + 2)
      else if at i ',' then (count, i + 1)
      else (count, i)
    in
    Some (go 1 1)

let sep_end_by1_prop =
  Prop.test
    ~name:"sep_end_by1 matches one or more items and a trailing separator"
    ~print:String.escaped
    Gen.(string_size ~gen:(oneof_list [ 'a'; 'a'; ','; 'x' ]) (Prop.bound 10))
    (fun s ->
      let p = P.sep_end_by1 ~sep:(P.char ',') (P.char 'a') in
      match (P.parse_prefix p s, sep_end_by1_model s) with
      | Ok (l, pos), Some (count, stop) -> List.length l = count && pos = stop
      | Error _, None -> true
      | _ -> false)

(* f. depth *)

let nest =
  P.fix (fun self ->
      P.(char '(' *> self <* char ')') |> fun p -> P.(p <|> char 'x'))

let depth_result ?max_depth levels =
  let s = String.make (levels - 1) '(' ^ "x" ^ String.make (levels - 1) ')' in
  P.parse ?max_depth nest s

let too_deep n = Printf.sprintf "nesting too deep (more than %d levels)" n

let depth_limit =
  Prop.test ~name:"depth: limit is enforced exactly" ~print:string_of_int
    (Gen.int_range 1 120) (fun levels ->
      match depth_result ~max_depth:50 levels with
      | Ok _ -> levels <= 50
      | Error [ d ] -> levels > 50 && P.Diagnostic.message d = too_deep 50
      | Error _ -> false)

let depth_default =
  Prop.once ~name:"depth: default limit is 5000" (fun () ->
      (match depth_result P.default_max_depth with Ok _ -> true | _ -> false)
      &&
      match depth_result (P.default_max_depth + 1) with
      | Error [ d ] -> P.Diagnostic.message d = too_deep 5000
      | _ -> false)

(* Entering a recursion point counts while it runs: shallow entries that
   follow one another, including those of failed alternatives, must not add
   up. [total] entries pass the limit, the nesting never does. *)
type recursion = Fix | Force | Delay

let enter_once api p =
  match api with
  | Fix -> P.fix (fun _ -> p)
  | Force -> P.force (lazy p)
  | Delay -> P.delay (fun () -> p)

let show_recursion = function
  | Fix -> "fix"
  | Force -> "force"
  | Delay -> "delay"

let depth_restored =
  Prop.test ~name:"depth: restored on exit; only nesting counts, not entries"
    ~print:(fun (api, levels, items) ->
      Printf.sprintf "%s, %d levels, %d items" (show_recursion api) levels items)
    Gen.(
      triple (oneof_list [ Fix; Force; Delay ]) (int_range 1 4) (int_range 1 8))
    (fun (api, levels, items) ->
      let rec nest k p = if k = 0 then p else nest (k - 1) (enter_once api p) in
      (* The first alternative enters [levels] deep, then fails. *)
      let item =
        P.(nest levels (char 'a' *> char 'b') <|> nest levels (char 'a'))
      in
      let input = String.make items 'a' in
      let run max_depth = P.parse ~max_depth (P.many item) input in
      run levels = Ok (List.init items (fun _ -> 'a'))
      &&
      match run (levels - 1) with
      | Error [ d ] -> P.Diagnostic.message d = too_deep (levels - 1)
      | _ -> false)

(* g. spanned, validate, context *)

let is_a c = c = 'a'
let gen_input = G.gen_input

let spanned_prop =
  Prop.test ~name:"spanned covers exactly the consumed bytes" ~print:print_gi
    (gen_gi ~cuts:true) (fun (g, s) ->
      let p =
        P.(
          let+ start = skip_while is_a *> position
          and+ _, sp = spanned (G.compile g)
          and+ stop = position in
          (start, stop, sp))
      in
      match P.parse_prefix p s with
      | Ok ((start, stop, sp), _) -> sp.start = start && sp.stop = stop
      | Error _ -> true
      | exception P.Grammar_error _ -> true)

let validate_prop =
  let is_ab c = c = 'a' || c = 'b' in
  let p =
    P.(
      skip_while (fun c -> c = 'c')
      *> validate
           (fun w -> if String.length w > 2 then Error "too long" else Ok w)
           (take_while1 "word" is_ab))
  in
  Prop.test ~name:"validate rejects with the consumed span"
    ~print:(Printf.sprintf "%S") gen_input (fun s ->
      let n = String.length s in
      let rec skip i c = if i < n && c s.[i] then skip (i + 1) c else i in
      let k = skip 0 (fun c -> c = 'c') in
      let e = skip k is_ab in
      match P.parse_prefix p s with
      | Ok (w, pos) ->
          e - k <= 2 && e > k && w = String.sub s k (e - k) && pos = e
      | Error [ d ] when e - k > 2 ->
          let sp = P.Diagnostic.span d in
          P.Diagnostic.message d = "too long" && sp.start = k && sp.stop = e
      | Error [ d ] -> e = k && P.Diagnostic.message d <> "too long"
      | Error _ -> false)

let context_prop =
  let p =
    P.(
      context "outer" (skip_while is_a *> context "inner" (char 'b' *> char 'c')))
  in
  Prop.test ~name:"context frames: innermost first, only if entered"
    ~print:(Printf.sprintf "%S") gen_input (fun s ->
      let n = String.length s in
      let rec skip i = if i < n && is_a s.[i] then skip (i + 1) else i in
      let k = skip 0 in
      match P.parse_prefix p s with
      | Ok _ -> true
      | Error [ d ] ->
          (* where it fails follows from the input, not from the report *)
          let e = if k < n && s.[k] = 'b' then k + 1 else k in
          (P.Diagnostic.span d).start = e
          &&
          let sp a =
            if e > a then P.Span.make ~start:a ~stop:e else P.Span.point a
          in
          let frames =
            (if e > k then [ ("inner", sp k) ] else [])
            @ if e > 0 then [ ("outer", sp 0) ] else []
          in
          let ctx = P.Diagnostic.context d in
          List.length ctx = List.length frames
          && List.for_all2
               (fun (a, x) (b, y) -> a = b && P.Span.equal x y)
               ctx frames
      | Error _ -> false)

(* Alternatives that fail at the same place, each inside its own contexts:
   the report keeps the first alternative's that has any. *)
let tied_context =
  let names = [ "p"; "q"; "r" ] in
  Prop.test ~name:"alt/choice: tied failures keep the earlier non-empty context"
    ~print:(fun (alts, use_choice) ->
      Printf.sprintf "%s%s"
        (String.concat " | " (List.map (String.concat ">") alts))
        (if use_choice then " (choice)" else ""))
    Gen.(
      pair
        (list_size (int_range 2 4) (list_size (int_bound 2) (oneof_list names)))
        bool)
    (fun (alts, use_choice) ->
      (* Alternative [i] consumes 'x', then fails expecting its own letter. *)
      let alt i frames =
        List.fold_right
          (fun name p -> P.context name p)
          frames
          P.(char 'x' *> char (Char.chr (Char.code 'a' + i)))
      in
      let parsers = List.mapi alt alts in
      let p =
        if use_choice then P.choice parsers
        else List.fold_left P.( <|> ) (List.hd parsers) (List.tl parsers)
      in
      let first = List.find_opt (fun frames -> frames <> []) alts in
      match P.parse p "xz" with
      | Error [ d ] ->
          P.Diagnostic.context d
          = List.rev_map
              (fun name -> (name, P.Span.make ~start:0 ~stop:1))
              (Option.value first ~default:[])
          && (P.Diagnostic.span d).start = 1
      | _ -> false)

(* [label] rewrites what happens at its start position and nothing deeper:
   failures there, and the hints a success leaves there. *)
let label_scope =
  Prop.test ~name:"label: rewrites failures and hints at its start only"
    ~print:(fun (hint, k, j) ->
      Printf.sprintf "%s, %d letters, fails at %d"
        (if hint then "hint" else "failure")
        k j)
    Gen.(triple bool (int_bound 3) (int_bound 3))
    (fun (hint, k, j) ->
      let lit c = P.Expected.Literal (String.make 1 c) in
      let a_s = List.init k (fun _ -> P.(char 'a' >>| ignore)) in
      let letters =
        List.fold_left (fun acc p -> P.(acc *> p)) (P.return ()) a_s
      in
      if hint then
        (* [label "L" (a^k [b])] then 'c': the hint of the optional 'b' sits at
           position k *)
        let p =
          P.(label "L" (letters *> optional (char 'b')) *> char 'c' >>| ignore)
        in
        match P.parse p (String.make k 'a' ^ "x") with
        | Error [ d ] ->
            P.Diagnostic.expected d
            = List.sort_uniq P.Expected.compare
                [ (if k = 0 then P.Expected.Label "L" else lit 'b'); lit 'c' ]
        | _ -> false
      else
        (* [label "L" (a^k b)] failing at position j <= k *)
        let j = min j k in
        let p = P.(label "L" (letters *> char 'b') >>| ignore) in
        match P.parse p (String.make j 'a' ^ "x") with
        | Error [ d ] ->
            P.Diagnostic.expected d
            = [
                (if j = 0 then P.Expected.Label "L"
                 else if j < k then lit 'a'
                 else lit 'b');
              ]
            && (P.Diagnostic.span d).start = j
        | _ -> false)

(* Commitment inside [attempt] does not escape it, whether the body succeeds
   or fails. *)
let attempt_absorbs_commitment =
  Prop.test ~name:"attempt: a cut inside does not commit what encloses it"
    ~print:(fun (succeeds, input) -> Printf.sprintf "%b on %S" succeeds input)
    Gen.(pair bool (oneof_list [ "ab"; "ac" ]))
    (fun (succeeds, input) ->
      let body =
        P.(
          char 'a' *> cut *> if succeeds then return () else char 'z' >>| ignore)
      in
      let p =
        P.(
          attempt body *> char 'b'
          >>| ignore
          <|> (char 'a' *> char 'c' >>| ignore))
      in
      Result.is_ok (P.parse p input)
      = (input = "ac" || (succeeds && input = "ab")))

(* Failures at the same place: a custom message beats expectations (the first
   custom one if there are several), otherwise the expectations are united. *)
let tied_failures =
  Prop.test ~name:"alt/choice: tied failures keep the first custom message"
    ~print:(fun (alts, use_choice) ->
      String.concat " | "
        (List.map (function Ok m -> m | Error c -> String.make 1 c) alts)
      ^ if use_choice then " (choice)" else "")
    Gen.(
      pair
        (list_size (int_range 2 4)
           (oneof
              [
                map (fun m -> Ok m) (oneof_list [ "m1"; "m2"; "m3" ]);
                map (fun c -> Error c) (oneof_list [ 'a'; 'b'; 'c' ]);
              ]))
        bool)
    (fun (alts, use_choice) ->
      let alt = function
        | Ok m -> P.fail m
        | Error c -> P.(char c >>| ignore)
      in
      let parsers = List.map (fun a -> P.(alt a >>| ignore)) alts in
      let p =
        if use_choice then P.choice parsers
        else List.fold_left P.( <|> ) (List.hd parsers) (List.tl parsers)
      in
      match P.parse p "z" with
      | Error [ d ] -> (
          match List.find_map (function Ok m -> Some m | _ -> None) alts with
          | Some m -> P.Diagnostic.message d = m
          | None ->
              P.Diagnostic.expected d
              = List.sort_uniq P.Expected.compare
                  (List.filter_map
                     (function
                       | Error c -> Some (P.Expected.Literal (String.make 1 c))
                       | Ok _ -> None)
                     alts))
      | _ -> false)

(* h. message phrasing *)

let message p s =
  match P.parse p s with
  | Error [ d ] -> P.Diagnostic.message d
  | Ok _ -> "<ok>"
  | Error _ -> "<many>"

let is_lower c = c >= 'a' && c <= 'z'

let phrasing =
  let c = P.char and ( <|> ) = P.( <|> ) in
  Prop.once ~name:"message phrasing" (fun () ->
      List.for_all
        (fun (got, want) -> got = want)
        [
          (message (P.not_followed_by (c 'a')) "a", "unexpected 'a'");
          (message (c 'x') "y", "expected 'x', found 'y'");
          (message (c 'x' <|> c 'y') "z", "expected 'x' or 'y', found 'z'");
          ( message (c 'x' <|> c 'y' <|> c 'z') "w",
            "expected 'x', 'y', or 'z', found 'w'" );
          (message (c 'x') "", "expected 'x', found end of input");
          ( message P.(hidden (skip_many (char ' ')) *> char 'x') "  y",
            "expected 'x', found 'y'" );
          ( message P.(string "let" <|> string "list") "lisx",
            {|expected "let" or "list", found "lisx"|} );
        ]
      && (match
            P.parse
              P.(
                not_followed_by (string "let")
                *> take_while1 "identifier" is_lower)
              "letter"
          with
        | Error [ d ] ->
            P.Diagnostic.message d = {|unexpected "let"|}
            && P.Span.equal (P.Diagnostic.span d) (P.Span.make ~start:0 ~stop:3)
        | _ -> false)
      &&
      (* Angstrom #226: ordered choice is not revisited *)
      Result.is_error P.(parse (string "a" <|> string "ab" <* eof) "ab"))

let codes =
  Prop.once ~name:"codes reach diagnostics; count (-1) is rejected" (fun () ->
      let code p s =
        match P.parse p s with Error [ d ] -> P.Diagnostic.code d | _ -> None
      in
      code (P.fail ~code:"E1" "boom") "" = Some "E1"
      && code (P.validate ~code:"E2" (fun _ -> Error "no") (P.char 'a')) "a"
         = Some "E2"
      && code (P.fail "boom") "" = None
      &&
      match P.count (-1) (P.char 'a') with
      | exception Invalid_argument _ -> true
      | _ -> false)

(* Secondary labels and notes *)

let gen_annotations =
  let open Gen in
  pair
    (list_size (int_bound 3)
       (map2
          (fun a l -> (P.Span.make ~start:a ~stop:(a + l), "label"))
          (int_bound 5) (int_bound 3)))
    (list_size (int_bound 2) (oneof_list [ "n1"; "n2"; "n3" ]))

let fail_annotations =
  Prop.test ~name:"fail: labels and notes reach the diagnostic, also recovered"
    ~print:(fun (labels, notes) ->
      Printf.sprintf "%d labels, notes %s" (List.length labels)
        (String.concat "," notes))
    gen_annotations
    (fun (labels, notes) ->
      let span = P.Span.make ~start:(List.length labels) ~stop:7 in
      let boom = P.fail ~span ~labels ~notes "boom" in
      let same d =
        P.Diagnostic.labels d = labels
        && P.Diagnostic.notes d = notes
        && P.Diagnostic.span d = span
      in
      let recovered =
        P.(
          recover ~catch:`Any ~sync:(skip_to eof)
            ~fallback:(fun _ -> 'r')
            (any *> boom))
      in
      (match P.parse boom "" with Error [ d ] -> same d | _ -> false)
      && (match P.parse P.(boom <|> char 'x') "" with
        | Error [ d ] -> same d
        | _ -> false)
      &&
      match P.parse_partial recovered "ab" with
      | Some 'r', [ d ] -> same d
      | _ -> false)

let between_opened_here =
  Prop.test
    ~name:
      "between: \"opened here\" exactly when the closer's failure is reported \
       and the body consumed"
    ~print:(Printf.sprintf "%S")
    Gen.(
      string_size ~gen:(oneof_list [ '('; ')'; 'a'; 'b'; ' ' ]) (int_bound 6))
    (fun s ->
      let p = P.(between (char '(') (char ')') (many letter)) in
      let n = String.length s in
      let stop =
        let rec go i =
          if i < n && s.[i] >= 'a' && s.[i] <= 'z' then go (i + 1) else i
        in
        go 1
      in
      let closed = n > 0 && s.[0] = '(' && stop < n && s.[stop] = ')' in
      let opened = n > 0 && s.[0] = '(' && (not closed) && stop > 1 in
      match P.parse_prefix p s with
      | Ok _ -> closed
      | Error [ d ] ->
          P.Diagnostic.labels d
          =
          if opened then [ (P.Span.make ~start:0 ~stop:1, "opened here") ]
          else []
      | Error _ -> false)

(* e. dispatch *)

(* Everything observable: the value and position, or all the diagnostics. *)
let obs_full_diag p s =
  match P.parse_prefix p s with
  | Ok r -> `Ok r
  | Error ds -> `Error ds
  | exception P.Grammar_error m -> `Grammar_error m

(* Each of 'a', 'b', 'c' maps to a generated parser or to nothing. *)
let gen_dispatch =
  let g = Gen.option ~ratio:0.25 (G.gen ~cuts:true) in
  Gen.(
    pair
      (pair (oneof_list [ "value"; "item" ]) (tup3 g g g))
      (pair (oneof_list [ ""; "a"; "ab"; "b"; "ca" ]) G.gen_input))

let print_dispatch ((name, (a, b, c)), (prefix, s)) =
  let show = function None -> "-" | Some g -> G.to_string g in
  Printf.sprintf "%s: a=%s b=%s c=%s on %S%S" name (show a) (show b) (show c)
    prefix s

let table (a, b, c) ch =
  Option.map G.compile
    (match ch with 'a' -> a | 'b' -> b | 'c' -> c | _ -> None)

(* [p] followed by a failure, so that [p]'s success hints are reported. *)
let then_z p = P.(p <* char 'z')

let dispatch_definition =
  Prop.test ~name:"dispatch: agrees with its definition" ~print:print_dispatch
    gen_dispatch (fun ((name, gs), (prefix, s)) ->
      let f = table gs in
      let d = P.dispatch ~label:name f in
      let reference =
        P.(
          label name
            ( peek >>= function
              | Some t -> ( match f t with Some p -> p | None -> choice [])
              | None -> choice [] ))
      in
      (* Alone, and after something that consumes (the failure position, the
         enclosing context and the hints it can carry all differ). *)
      let wrap p = P.(context "wrap" (string prefix *> p)) in
      let input = prefix ^ s in
      List.for_all
        (fun (a, b) -> obs_full_diag a input = obs_full_diag b input)
        [
          (d, reference);
          (wrap d, wrap reference);
          (* a failure after it shows the hints a success leaves *)
          (then_z d, then_z reference);
        ])

(* Alternatives that each start with their own token are the case where
   [dispatch] is a faster [choice ~label]. *)
let dispatch_is_choice =
  Prop.test ~name:"dispatch: equals choice ~label on disjoint first tokens"
    ~print:print_dispatch gen_dispatch (fun ((name, gs), (prefix, s)) ->
      let f = table gs in
      let alternative ch = Option.map (fun p -> P.(char ch *> p)) (f ch) in
      let alternatives = List.filter_map alternative [ 'a'; 'b'; 'c' ] in
      let dispatch =
        P.dispatch ~label:name (fun ch ->
            Option.map (fun p -> P.(char ch *> p)) (f ch))
      in
      let choice = P.choice ~label:name alternatives in
      let wrap p = P.(context "wrap" (string prefix *> p)) in
      let input = prefix ^ s in
      List.for_all
        (fun (a, b) -> obs_full_diag a input = obs_full_diag b input)
        [
          (dispatch, choice);
          (wrap dispatch, wrap choice);
          (then_z dispatch, then_z choice);
        ])

let () =
  Prop.run "engine"
    ([ outcomes_agree; errors_agree ~cuts:false; errors_agree ~cuts:true ]
    @ law_tests
    @ [
        cut_never_changes_success;
        stack_safety;
        sep_end_by1_prop;
        depth_limit;
        depth_default;
        depth_restored;
        spanned_prop;
        validate_prop;
        context_prop;
        tied_context;
        tied_failures;
        label_scope;
        attempt_absorbs_commitment;
        phrasing;
        codes;
        fail_annotations;
        between_opened_here;
        dispatch_definition;
        dispatch_is_choice;
      ])
