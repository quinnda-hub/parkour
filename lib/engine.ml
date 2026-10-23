(* The parsing engine, generic over its input. See PLAN.md §3 for the
   semantics this implements; the comments here only explain the mechanics. *)

open Parkour_intf

module Make (I : INPUT) = struct
  type input = I.t
  type token = I.token

  (* Expectation sets are merged in O(1) and only flattened, sorted and
     deduplicated when a diagnostic is built. *)
  type expectations =
    | Items of Expected.t list
    | Union of expectations * expectations

  (* [unexpected]: "unexpected X", where X is the input from [pos] up to this
     index. It is reported only when there are no expectations, but it is
     kept alongside them so that [hidden] can remove the expectations alone. *)
  type kind =
    | Expect of { items : expectations; unexpected : int option }
    | Custom of { code : string option; message : string }

  type error = {
    pos : int;  (** input index; the key for furthest-failure merging *)
    kind : kind;
    span : Span.t option;  (** display span of a [Custom] error *)
    context : (string * int) list;  (** outermost first; start indices *)
  }

  type state = { input : I.t; len : int; max_depth : int; mutable depth : int }

  (* [cut]: a commitment point was crossed inside this parser.
     [hint]: the furthest uncommitted failure seen on the way to this success,
     kept only while it is at or beyond [pos]. *)
  type 'a reply =
    | Pass of { value : 'a; pos : int; cut : bool; hint : error option }
    | Fail of { error : error; cut : bool }

  type 'a t = state -> int -> 'a reply

  exception Too_deep of int

  (* Errors *)

  let expect_error pos items =
    {
      pos;
      kind = Expect { items = Items items; unexpected = None };
      span = None;
      context = [];
    }

  let flatten e =
    let rec go acc = function
      | [] -> acc
      | Items l :: rest -> go (List.rev_append l acc) rest
      | Union (a, b) :: rest -> go acc (a :: b :: rest)
    in
    List.sort_uniq Expected.compare (go [] [ e ])

  (* [merge earlier later]: the furthest error wins. At the same position a
     custom message beats everything (the earlier one if both are custom);
     otherwise expectations are combined and the earlier "unexpected X" is
     kept. *)
  let merge a b =
    if a.pos > b.pos then a
    else if b.pos > a.pos then b
    else
      let context = if a.context <> [] then a.context else b.context in
      match (a.kind, b.kind) with
      | Custom _, _ -> { a with context }
      | _, Custom _ -> { b with context }
      | Expect x, Expect y ->
          let unexpected =
            match x.unexpected with None -> y.unexpected | u -> u
          in
          {
            pos = a.pos;
            kind = Expect { items = Union (x.items, y.items); unexpected };
            span = None;
            context;
          }

  let join h1 h2 =
    match (h1, h2) with
    | None, h | h, None -> h
    | Some a, Some b -> Some (merge a b)

  let with_hint hint e = match hint with None -> e | Some h -> merge h e
  let keep pos = function Some h when h.pos >= pos -> Some h | _ -> None
  let pass value pos = Pass { value; pos; cut = false; hint = None }
  let fail_at pos items = Fail { error = expect_error pos items; cut = false }

  let source_span st start stop =
    if stop <= start then Span.point (I.span st.input start).start
    else Span.union (I.span st.input start) (I.span st.input (stop - 1))

  (* Running *)

  let default_max_depth = 5000

  let found_at st pos =
    if pos < st.len then I.unexpected st.input pos
    else ("end of input", I.span st.input st.len)

  let expected_message items found =
    match List.map Expected.to_string items with
    | [] -> "unexpected " ^ found
    | [ a ] -> Printf.sprintf "expected %s, found %s" a found
    | [ a; b ] -> Printf.sprintf "expected %s or %s, found %s" a b found
    | l ->
        let rev = List.rev l in
        Printf.sprintf "expected %s, or %s, found %s"
          (String.concat ", " (List.rev (List.tl rev)))
          (List.hd rev) found

  let to_diagnostic st e =
    let context =
      List.rev_map
        (fun (name, start) -> (name, source_span st start e.pos))
        e.context
    in
    let expected_diagnostic expected =
      let found, span = found_at st e.pos in
      Diagnostic.make ~expected ~found ~context ~span
        (expected_message expected found)
    in
    match e.kind with
    | Expect { items; unexpected } -> (
        match (flatten items, unexpected) with
        | [], Some stop when stop > e.pos ->
            let found = I.describe st.input e.pos stop in
            Diagnostic.make ~found ~context
              ~span:(source_span st e.pos stop)
              ("unexpected " ^ found)
        | expected, _ -> expected_diagnostic expected)
    | Custom { code; message } ->
        let span =
          match e.span with
          | Some s -> s
          | None -> Span.point (I.span st.input e.pos).start
        in
        Diagnostic.make ?code ~context ~span message

  let run max_depth p input =
    let st = { input; len = I.length input; max_depth; depth = 0 } in
    match p st 0 with
    | Pass r -> Ok (r.value, r.pos)
    | Fail r -> Error [ to_diagnostic st r.error ]
    | exception Too_deep pos ->
        let span = Span.point (I.span st.input (min pos st.len)).start in
        Error
          [
            Diagnostic.make ~span
              (Printf.sprintf "nesting too deep (more than %d levels)" max_depth);
          ]

  (* Basic combinators *)

  let return x _ pos = pass x pos

  let fail ?code message _ pos =
    Fail
      {
        error =
          { pos; kind = Custom { code; message }; span = None; context = [] };
        cut = false;
      }

  let map f p st pos =
    match p st pos with
    | Pass r ->
        Pass { value = f r.value; pos = r.pos; cut = r.cut; hint = r.hint }
    | Fail r -> Fail r

  (* Run [p] then [q] from where [p] stopped, combining the values with [k].
     Commitment accumulates; hints from [p] enrich [q]'s failure. *)
  let seq p q k st pos =
    match p st pos with
    | Fail r -> Fail r
    | Pass a -> (
        match q st a.pos with
        | Pass b ->
            Pass
              {
                value = k a.value b.value;
                pos = b.pos;
                cut = a.cut || b.cut;
                hint = keep b.pos (join a.hint b.hint);
              }
        | Fail b ->
            Fail { error = with_hint a.hint b.error; cut = a.cut || b.cut })

  let bind p f st pos =
    match p st pos with
    | Fail r -> Fail r
    | Pass a -> (
        match f a.value st a.pos with
        | Pass b when Option.is_none a.hint && not a.cut -> Pass b
        | Pass b ->
            Pass
              {
                value = b.value;
                pos = b.pos;
                cut = a.cut || b.cut;
                hint = keep b.pos (join a.hint b.hint);
              }
        | Fail b ->
            Fail { error = with_hint a.hint b.error; cut = a.cut || b.cut })

  let both p q = seq p q (fun a b -> (a, b))

  (* Ordered choice. A committed result of either side is final, and a
     committed [q] discards [p]'s failure: once committed, alternatives that
     were not taken are no longer relevant to the report. *)
  let alt p q st pos =
    match p st pos with
    | Pass _ as r -> r
    | Fail { cut = true; _ } as r -> r
    | Fail a -> (
        match q st pos with
        | Pass b when not b.cut ->
            Pass
              {
                value = b.value;
                pos = b.pos;
                cut = false;
                hint = keep b.pos (join (Some a.error) b.hint);
              }
        | Fail b when not b.cut ->
            Fail { error = merge a.error b.error; cut = false }
        | r -> r)

  (* Describing failures *)

  (* [label] describes a failure at its start position by its name, whatever
     the failure said (unless it is a custom message). *)
  let label name p =
    let relabel e =
      match e.kind with
      | Expect _ ->
          {
            e with
            kind =
              Expect
                { items = Items [ Expected.Label name ]; unexpected = None };
          }
      | Custom _ -> e
    in
    fun st pos ->
      match p st pos with
      | Pass ({ hint = Some h; _ } as r) when h.pos = pos ->
          Pass
            {
              value = r.value;
              pos = r.pos;
              cut = r.cut;
              hint = Some (relabel h);
            }
      | Fail r when r.error.pos = pos ->
          Fail { error = relabel r.error; cut = r.cut }
      | r -> r

  (* [hidden] contributes nothing to later errors when it succeeds, and no
     expectations when it fails at its start position. *)
  let hidden p st pos =
    match p st pos with
    | Pass { hint = Some _; value; pos = stop; cut } ->
        Pass { value; pos = stop; cut; hint = None }
    | Fail { error = { kind = Expect { unexpected; _ }; _ } as e; cut }
      when e.pos = pos ->
        Fail
          {
            error = { e with kind = Expect { items = Items []; unexpected } };
            cut;
          }
    | r -> r

  (* Frames are recorded only for failures past the construct's start: a
     construct that failed on its first token was never really entered (that
     case is [label]'s job). Frames are pushed while unwinding, so the list
     ends up outermost first. *)
  let context name p st pos =
    let add e =
      if e.pos > pos then { e with context = (name, pos) :: e.context } else e
    in
    match p st pos with
    | Pass ({ hint = Some h; _ } as r) ->
        Pass { value = r.value; pos = r.pos; cut = r.cut; hint = Some (add h) }
    | Pass _ as r -> r
    | Fail r -> Fail { error = add r.error; cut = r.cut }

  let validate ?code check p st pos =
    match p st pos with
    | Fail r -> Fail r
    | Pass r -> (
        match check r.value with
        | Ok value -> Pass { value; pos = r.pos; cut = r.cut; hint = r.hint }
        | Error message ->
            let e =
              {
                pos = r.pos;
                kind = Custom { code; message };
                span = Some (source_span st pos r.pos);
                context = [];
              }
            in
            Fail { error = with_hint r.hint e; cut = r.cut })

  (* Operators *)

  module Syntax = struct
    let ( >>= ) = bind
    let ( >>| ) p f = map f p
    let ( <$> ) = map
    let ( <*> ) pf p = seq pf p (fun f x -> f x)
    let ( *> ) p q = seq p q (fun _ b -> b)
    let ( <* ) p q = seq p q (fun a _ -> a)
    let ( <|> ) = alt
    let ( <?> ) p name = label name p
    let ( let* ) = bind
    let ( let+ ) p f = map f p
    let ( and+ ) = both
  end

  include Syntax

  (* Choice and commitment *)

  let cut _ pos = Pass { value = (); pos; cut = true; hint = None }

  let attempt p st pos =
    match p st pos with
    | Pass r when r.cut -> Pass { r with cut = false }
    | Fail r when r.cut -> Fail { r with cut = false }
    | r -> r

  (* Same as folding [alt], but iterative. *)
  let choice ?label:name ps =
    let p st pos =
      let rec go err = function
        | [] ->
            let error =
              match err with Some e -> e | None -> expect_error pos []
            in
            Fail { error; cut = false }
        | p :: rest -> (
            match p st pos with
            | Pass r when not r.cut -> (
                match err with
                | None -> Pass r
                | Some e ->
                    Pass { r with hint = keep r.pos (join (Some e) r.hint) })
            | Fail r when not r.cut ->
                go
                  (Some
                     (match err with
                     | None -> r.error
                     | Some e -> merge e r.error))
                  rest
            | r -> r)
      in
      go None ps
    in
    match name with None -> p | Some name -> label name p

  (* Repetition. Each loop is tail-recursive. An uncommitted failure ends the
     loop and is remembered as a hint; a committed failure fails the loop. *)

  let zero_width name pos =
    raise
      (Grammar_error
         (Printf.sprintf
            "Parkour.%s: the repeated parser succeeded without consuming input \
             at position %d, so it would repeat forever"
            name pos))

  let fold_loop name f init p st pos0 =
    let rec go acc pos cut hint =
      match p st pos with
      | Pass r ->
          if r.pos = pos then zero_width name pos;
          go (f acc r.value) r.pos (cut || r.cut)
            (keep r.pos (join hint r.hint))
      | Fail r when r.cut -> Fail { error = with_hint hint r.error; cut = true }
      | Fail r ->
          Pass
            {
              value = acc;
              pos;
              cut;
              hint = keep pos (join hint (Some r.error));
            }
    in
    go init pos0 false None

  let cons acc x = x :: acc
  let many p = map List.rev (fold_loop "many" cons [] p)

  let many1 p =
    seq p (fold_loop "many1" cons [] p) (fun x xs -> x :: List.rev xs)

  let fold_many f init p = fold_loop "fold_many" f init p
  let skip_many p = fold_loop "skip_many" (fun () _ -> ()) () p

  let skip_many1 p =
    seq p (fold_loop "skip_many1" (fun () _ -> ()) () p) (fun _ () -> ())

  let sep_by_named name ~sep p =
    seq p
      (fold_loop name cons [] (seq sep p (fun _ x -> x)))
      (fun x xs -> x :: List.rev xs)

  let sep_by1 ~sep p = sep_by_named "sep_by1" ~sep p
  let sep_by ~sep p = alt (sep_by_named "sep_by" ~sep p) (return [])

  let sep_end_by_named name ~sep p =
    let rest st pos0 =
      let rec go acc pos cut hint =
        match sep st pos with
        | Fail r when r.cut ->
            Fail { error = with_hint hint r.error; cut = true }
        | Fail r ->
            Pass
              {
                value = acc;
                pos;
                cut;
                hint = keep pos (join hint (Some r.error));
              }
        | Pass s -> (
            let cut = cut || s.cut and hint = keep s.pos (join hint s.hint) in
            match p st s.pos with
            | Pass r ->
                if r.pos = pos then zero_width name pos;
                go (r.value :: acc) r.pos (cut || r.cut)
                  (keep r.pos (join hint r.hint))
            | Fail r when r.cut ->
                Fail { error = with_hint hint r.error; cut = true }
            | Fail r ->
                Pass
                  {
                    value = acc;
                    pos = s.pos;
                    cut;
                    hint = keep s.pos (join hint (Some r.error));
                  })
      in
      go [] pos0 false None
    in
    seq p rest (fun x xs -> x :: List.rev xs)

  let sep_end_by1 ~sep p = sep_end_by_named "sep_end_by1" ~sep p
  let sep_end_by ~sep p = alt (sep_end_by_named "sep_end_by" ~sep p) (return [])

  let count n p =
    if n < 0 then invalid_arg "Parkour.count: negative count";
    fun st pos0 ->
      let rec go i acc pos cut hint =
        if i = 0 then Pass { value = List.rev acc; pos; cut; hint }
        else
          match p st pos with
          | Pass r ->
              go (i - 1) (r.value :: acc) r.pos (cut || r.cut)
                (keep r.pos (join hint r.hint))
          | Fail r ->
              Fail { error = with_hint hint r.error; cut = cut || r.cut }
      in
      go n [] pos0 false None

  (* Each step is a choice between [stop] and [p], so a committed [p] failure
     discards [stop]'s failure, as in [alt]. *)
  let many_till p stop st pos0 =
    let rec go acc pos cut hint =
      match stop st pos with
      | Pass s ->
          Pass
            {
              value = List.rev acc;
              pos = s.pos;
              cut = cut || s.cut;
              hint = keep s.pos (join hint s.hint);
            }
      | Fail s when s.cut -> Fail { error = with_hint hint s.error; cut = true }
      | Fail s -> (
          match p st pos with
          | Pass r ->
              if r.pos = pos then zero_width "many_till" pos;
              let hint = if r.cut then hint else join hint (Some s.error) in
              go (r.value :: acc) r.pos (cut || r.cut)
                (keep r.pos (join hint r.hint))
          | Fail r when r.cut ->
              Fail { error = with_hint hint r.error; cut = true }
          | Fail r ->
              Fail { error = with_hint hint (merge s.error r.error); cut })
    in
    go [] pos0 false None

  let optional p = alt (map Option.some p) (return None)
  let option default p = alt p (return default)

  (* Lookahead and position *)

  let eof st pos =
    if pos >= st.len then pass () pos else fail_at pos [ Expected.End_of_input ]

  let lookahead p st pos =
    match p st pos with
    | Pass r -> pass r.value pos
    | Fail r -> Fail { r with cut = false }

  let not_followed_by p st pos =
    match p st pos with
    | Fail _ -> pass () pos
    | Pass r ->
        Fail
          {
            error =
              {
                pos;
                kind = Expect { items = Items []; unexpected = Some r.pos };
                span = None;
                context = [];
              };
            cut = false;
          }

  let position _ pos = pass pos pos

  let spanned p st pos =
    match p st pos with
    | Pass r ->
        Pass
          {
            value = (r.value, source_span st pos r.pos);
            pos = r.pos;
            cut = r.cut;
            hint = r.hint;
          }
    | Fail r -> Fail r

  (* Recursion. Depth is per run; an exception aborts the run, so it need not
     be restored on the exceptional path. *)

  let enter p st pos =
    if st.depth >= st.max_depth then raise_notrace (Too_deep pos);
    st.depth <- st.depth + 1;
    let r = p st pos in
    st.depth <- st.depth - 1;
    r

  let force l st pos = enter (Lazy.force l) st pos
  let delay f st pos = enter (f ()) st pos

  (* Forced here, so a parser built with [fix] is safe to share between
     domains from the start. *)
  let fix f =
    let rec l = lazy (f (force l)) in
    let (_ : 'a t) = Lazy.force l in
    force l

  (* Tokens *)

  let expect name f st pos =
    if pos < st.len then
      match f (I.get st.input pos) with
      | Some v -> pass v (pos + 1)
      | None -> fail_at pos [ Expected.Label name ]
    else fail_at pos [ Expected.Label name ]

  let satisfy name pred st pos =
    if pos < st.len && pred (I.get st.input pos) then
      pass (I.get st.input pos) (pos + 1)
    else fail_at pos [ Expected.Label name ]

  let token t =
    let items = [ Expected.Literal (I.show t) ] in
    fun st pos ->
      if pos < st.len && I.equal (I.get st.input pos) t then pass t (pos + 1)
      else fail_at pos items

  let any st pos =
    if pos < st.len then pass (I.get st.input pos) (pos + 1) else fail_at pos []

  let peek st pos =
    pass (if pos < st.len then Some (I.get st.input pos) else None) pos

  (* Running *)

  let parse_prefix ?(max_depth = default_max_depth) p input =
    run max_depth p input

  let parse ?(max_depth = default_max_depth) p input =
    match run max_depth (seq p eof (fun x () -> x)) input with
    | Ok (x, _) -> Ok x
    | Error _ as e -> e
end
