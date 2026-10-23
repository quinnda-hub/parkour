(* Hardening tests: named regressions from other libraries' issue trackers,
   and an audit that every loop and every recursive construct is stack-safe,
   depth-limited or rejected. Sizes are fixed (not scaled by --long): each
   check runs once, in both tiers ([Prop.once]).

   Already covered elsewhere, and deliberately not repeated here:
   - test_engine.ml "stack safety on 10^6 items": many, many1, sep_by,
     sep_end_by, count (Angstrom #221), many_till, skip_many, fold_many.
   - test_text.ml "stack safety on 10 MB inputs": quoted strings with escapes,
     one huge line or block comment, 10^6 nested block comments, identifier,
     take_while, Utf8.take_while.
   - test_expr.ml "very long chains": expression, chainl1, chainr1 and
     prefix/postfix runs at 10^5; repeated here at 10^6.
   - test_source_render.ml "one error in a 10 MB single line". *)

module P = Parkour
module D = P.Diagnostic
module Prop = Parkour_test_support.Prop
open QCheck2

let rep n s = String.concat "" (List.init n (fun _ -> s))
let once name f = Prop.once ~name f
let big = 1_000_000
let too_deep n = Printf.sprintf "nesting too deep (more than %d levels)" n

(* Angstrom #209: stack overflow on deeply nested JSON *)

type json = Array of json list | Num of float

let rec value =
  lazy P.(choice ~label:"value" [ (number >>| fun n -> Num n); force array ])

and array =
  lazy
    P.(
      context "array"
        ( between (char '[') (char ']')
            (cut *> sep_by ~sep:(char ',') (force value))
        >>| fun items -> Array items ))

let json = P.force value
let nested n = rep n "[" ^ rep n "]"

let angstrom_209 =
  once "Angstrom #209: deeply nested JSON parses or reports depth" (fun () ->
      (match P.parse json (nested 300) with Ok _ -> true | Error _ -> false)
      &&
      match P.parse json (nested big) with
      | Error [ d ] -> D.message d = too_deep P.default_max_depth
      | _ -> false)

(* Angstrom #226: ordered choice surprise. First success wins, a later failure
   does not revisit it. *)

type expr = Var of string | Plus of expr * expr | App of expr * expr

let space = P.skip_while (fun c -> c = ' ' || c = '\t' || c = '\n')

let grammar ~arith_first =
  P.fix (fun _ ->
      let open P in
      let atom = take_while1 "variable" is_letter >>| fun v -> Var v in
      let plus =
        fix (fun plusp ->
            atom <* space
            <* (char '+' <* space)
            >>= (fun a -> plusp >>| fun e -> Plus (a, e))
            <|> atom)
      in
      let arith = plus <|> atom in
      let app =
        arith <* space >>= fun e1 ->
        arith >>| fun e2 -> App (e1, e2)
      in
      if arith_first then arith <|> app else app <|> arith)

let angstrom_226 =
  once "Angstrom #226: ordered choice is PEG-like" (fun () ->
      (match P.parse (grammar ~arith_first:false) "a b" with
        | Ok e -> e = App (Var "a", Var "b")
        | Error _ -> false)
      &&
      match P.parse (grammar ~arith_first:true) "a b" with
      | Error [ d ] ->
          Format.asprintf "%a" (D.pp_located (P.Source.make "a b")) d
          = "1:3: error: expected '+', found 'b'"
      | _ -> false)

(* Zero-width bodies: every open-ended repetition rejects a body that succeeds
   without consuming, instead of looping. *)

exception Runaway

let zero_width =
  (* A body that gives up after [fuel] runs: a loop that fails to reject it
     fails the property instead of hanging the suite. *)
  let fuel = 10_000 in
  let runs = ref 0 in
  let z =
    P.map
      (fun () ->
        incr runs;
        if !runs > fuel then raise Runaway)
      (P.return ())
  in
  let cases =
    [
      ("many", fun s -> P.parse (P.many z) s |> ignore);
      ("many1", fun s -> P.parse (P.many1 z) s |> ignore);
      ("skip_many", fun s -> P.parse (P.skip_many z) s |> ignore);
      ("skip_many1", fun s -> P.parse (P.skip_many1 z) s |> ignore);
      ( "fold_many",
        fun s -> P.parse (P.fold_many (fun a () -> a) 0 z) s |> ignore );
      ("sep_by", fun s -> P.parse (P.sep_by ~sep:z z) s |> ignore);
      ("sep_by1", fun s -> P.parse (P.sep_by1 ~sep:z z) s |> ignore);
      ("sep_end_by", fun s -> P.parse (P.sep_end_by ~sep:z z) s |> ignore);
      ("many_till", fun s -> P.parse (P.many_till z (P.char '!')) s |> ignore);
      ( "chainl1",
        fun s -> P.parse (P.chainl1 z (P.return (fun () () -> ()))) s |> ignore
      );
      ( "chainr1",
        fun s -> P.parse (P.chainr1 z (P.return (fun () () -> ()))) s |> ignore
      );
      ( "expression",
        fun s ->
          P.parse
            (P.expression z [ [ P.Postfix (P.map (fun () -> Fun.id) z) ] ])
            s
          |> ignore );
      ("tokenize", fun s -> P.tokenize z s |> ignore);
    ]
  in
  Prop.test ~name:"zero-width repetition body raises Grammar_error" ~print:fst
    (Gen.oneof_list cases) (fun (_, run) ->
      List.for_all
        (fun s ->
          runs := 0;
          match run s with
          | () -> false
          | exception P.Grammar_error _ -> true
          | exception Runaway -> false)
        [ ""; "a"; "ab" ])

(* Stack safety at 10^6 iterations of the loops the other suites do not
   reach (or reach with smaller inputs). *)

let loops_stack_safe =
  once "stack safety: sep_by1, skip_many1, chains and operators at 10^6"
    (fun () ->
      let one = P.(char '1' >>| fun _ -> 1) in
      let add = P.(char '+' >>| fun _ a b -> a + b) in
      let inc c = P.(char c >>| fun _ a -> a + 1) in
      let chain = "1" ^ rep (big - 1) "+1" in
      let ok n p s = P.parse p s = Ok n in
      (match P.parse (P.sep_by1 ~sep:(P.char ',') one) ("1" ^ rep big ",1") with
        | Ok l -> List.length l = big + 1
        | Error _ -> false)
      && ok () (P.skip_many1 (P.char 'a')) (String.make big 'a')
      && ok big (P.expression one [ [ P.Infix_left add ] ]) chain
      && ok big (P.expression one [ [ P.Infix_right add ] ]) chain
      && ok big (P.chainl1 one add) chain
      && ok big (P.chainr1 one add) chain
      && ok (big + 1)
           (P.expression one [ [ P.Prefix (inc '-') ] ])
           (String.make big '-' ^ "1")
      && ok (big + 1)
           (P.expression one [ [ P.Postfix (inc '!') ] ])
           ("1" ^ String.make big '!'))

let skipping_stack_safe =
  once "stack safety: skip_to and skip_past, flat and nested, at 10^6"
    (fun () ->
      let semi = P.char ';' and nested = (P.char '(', P.char ')') in
      let stops p s = P.parse_prefix p s |> Result.map snd in
      let flat = String.make big 'x' ^ ";" in
      let deep = rep big "(" ^ rep big ")" ^ ";" in
      stops P.(skip_to semi) flat = Ok big
      && stops P.(skip_past semi) flat = Ok (big + 1)
      && stops P.(skip_to ~nested semi) deep = Ok (2 * big)
      && stops P.(skip_past ~nested semi) deep = Ok ((2 * big) + 1)
      && stops
           P.(skip_to ~nested:(char '"', char '"') semi)
           ("\"" ^ rep big ";" ^ "\"" ^ ";")
         = Ok (big + 2))

(* Many recoveries: each line is [x!;] and recovers by skipping past [;]. *)
let recovery_stack_safe =
  once "stack safety: parse_partial with 10^5 recovered errors" (fun () ->
      let n = 100_000 in
      let item =
        P.(
          char 'x' *> cut *> char 'y'
          |> recover ~sync:(skip_past (char ';')) ~fallback:(fun _ -> 'e'))
      in
      let value, ds =
        P.parse_partial ~max_errors:(n + 1) (P.many item) (rep n "x!;")
      in
      List.length ds = n
      && (match value with Some l -> List.length l = n | None -> false)
      && Result.is_error (P.parse (P.many item) (rep n "x!;"))
      &&
      (* one long skip *)
      match
        P.parse_partial
          P.(item *> skip_many any)
          ("x" ^ String.make big '!' ^ ";")
      with
      | Some (), [ _ ] -> true
      | _ -> false)

(* Many consecutive comments (the 10 MB tests use one comment each). *)
let comments_stack_safe =
  once "stack safety: 10^5 consecutive comments in Lexeme.space" (fun () ->
      let module L =
        (val P.Lexeme.make ~line_comments:[ "#" ]
               ~block_comments:[ ("/*", "*/") ]
               ())
      in
      let module N =
        (val P.Lexeme.make
               ~block_comments:[ ("/*", "*/") ]
               ~nested_comments:true ())
      in
      let n = 200_000 in
      let ends p s = P.parse_prefix p s |> Result.map snd in
      let lines = rep n "# c\n" and blocks = rep n "/* c */ " in
      ends L.space lines = Ok (String.length lines)
      && ends L.space blocks = Ok (String.length blocks)
      && ends N.space blocks = Ok (String.length blocks))

let text_stack_safe =
  once "stack safety: tokenize 10^6 tokens, Utf8 parsers" (fun () ->
      let lexer = P.tokenize ~skip:(P.skip_while P.is_space) P.(char '7') in
      let utf8 = rep big "\xc3\xa9" in
      (match lexer (String.concat " " (List.init big (fun _ -> "7"))) with
        | Ok a -> Array.length a = big
        | Error _ -> false)
      && (match P.parse P.(many Utf8.any) utf8 with
        | Ok l -> List.length l = big
        | Error _ -> false)
      && P.parse P.(Utf8.skip_while (fun _ -> true)) utf8 = Ok ()
      &&
      match P.parse P.(Utf8.take_while1 "e" (fun _ -> true)) utf8 with
      | Ok s -> String.length s = 2 * big
      | Error _ -> false)

(* Recursive constructs are depth-limited or iterative. *)

let rec parens =
  lazy
    P.(
      expression
        (number <|> between (char '(') (char ')') (force parens))
        [ [ Infix_left (char '+' >>| fun _ a b -> a +. b) ] ])

let depth_limited =
  once "depth: nested expression parentheses are limited" (fun () ->
      let n = P.default_max_depth in
      let ok s = Result.is_ok (P.parse (P.force parens) s) in
      ok (rep 100 "(" ^ "1" ^ rep 100 ")")
      && (not (ok (rep n "(" ^ "1" ^ rep n ")")))
      &&
      match P.parse (P.force parens) (rep big "(") with
      | Error [ d ] -> D.message d = too_deep n
      | _ -> false)

let context_rendering =
  once "render: 10^5 context frames render without blowup" (fun () ->
      let frames =
        List.init 100_000 (fun i ->
            ("c" ^ string_of_int i, P.Span.make ~start:0 ~stop:1))
      in
      let d = D.make ~context:frames ~span:(P.Span.make ~start:0 ~stop:1) "m" in
      let source = P.Source.make "abc" in
      let lines r = List.length (String.split_on_char '\n' r) in
      lines (D.render source d) < 20
      && lines (D.render ~max_context:max_int source d) > 100_000)

let () =
  Prop.run "robustness"
    [
      angstrom_209;
      angstrom_226;
      zero_width;
      loops_stack_safe;
      skipping_stack_safe;
      recovery_stack_safe;
      comments_stack_safe;
      text_stack_safe;
      depth_limited;
      context_rendering;
    ]
