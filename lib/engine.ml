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

  (* What most errors do not have, in one block so that the common error is
     small: a plain expectation failure shares [no_extra]. *)
  type extra = {
    span : Span.t option;  (** display span of a [Custom] error *)
    context : (string * int) list;  (** outermost first; start indices *)
    labels : (Span.t * string) list;  (** secondary labels, source spans *)
    notes : string list;
  }

  type error = {
    pos : int;  (** input index; the key for furthest-failure merging *)
    kind : kind;
    extra : extra;
  }

  (* The constructs being parsed (see [context]), innermost first. One block
     per frame, so entering a context allocates once. *)
  type frames = No_frame | Frame of string * int * frames

  (* A recorded diagnostic. Recovery entries (from [recover], [or_insert] and
     the final failure, as opposed to [emit]) have [found], the source offset
     where the failure was found; they take part in dedupe. *)
  type entry = { diagnostic : Diagnostic.t; found : int option }

  (* [log]: the diagnostics recorded by recovery, newest first, and [errors]:
     how many of them are errors. Every point that backtracks restores both
     (two field writes), so abandoned branches leave nothing behind. *)
  type state = {
    input : I.t;
    len : int;
    max_depth : int;
    max_errors : int;
    mutable depth : int;
    mutable log : entry list;
    mutable errors : int;
    mutable frames : frames;
    mutable trivia_start : int;
    mutable trivia_stop : int;
  }

  (* [cut]: a commitment point was crossed inside this parser.
     [hint]: the furthest uncommitted failure seen on the way to this success,
     kept only while it is at or beyond [pos]. *)
  type 'a reply =
    | Pass of { value : 'a; pos : int; cut : bool; hint : error option }
    | Fail of { error : error; cut : bool }

  type 'a t = state -> int -> 'a reply

  exception Too_deep of int

  (* Errors *)

  let no_extra = { span = None; context = []; labels = []; notes = [] }

  (* The kind of a failure expecting [items]. Build it once, when the parser
     is built, rather than on every failure. *)
  let expect_kind items = Expect { items = Items items; unexpected = None }
  let nothing_expected = expect_kind []

  let expect_error pos items =
    { pos; kind = expect_kind items; extra = no_extra }

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
      let first l m = match l with [] -> m | _ -> l in
      let context = first a.extra.context b.extra.context in
      let with_context e =
        if context == e.extra.context then e
        else { e with extra = { e.extra with context } }
      in
      match (a.kind, b.kind) with
      | Custom _, _ -> with_context a
      | _, Custom _ -> with_context b
      | Expect x, Expect y ->
          let unexpected =
            match x.unexpected with None -> y.unexpected | u -> u
          in
          let extra =
            if a.extra == no_extra && b.extra == no_extra then no_extra
            else
              {
                span = None;
                context;
                labels = first a.extra.labels b.extra.labels;
                notes = first a.extra.notes b.extra.notes;
              }
          in
          {
            pos = a.pos;
            kind = Expect { items = Union (x.items, y.items); unexpected };
            extra;
          }

  let join h1 h2 =
    match (h1, h2) with
    | None, h | h, None -> h
    | Some a, Some b -> Some (merge a b)

  let with_hint hint e = match hint with None -> e | Some h -> merge h e
  let keep pos = function Some h as hint when h.pos >= pos -> hint | _ -> None

  (* [keep pos (join h1 h2)], without merging hints that are dropped anyway. *)
  let keep_join pos h1 h2 = join (keep pos h1) (keep pos h2)
  let pass value pos = Pass { value; pos; cut = false; hint = None }

  let fail_kind pos kind =
    Fail { error = { pos; kind; extra = no_extra }; cut = false }

  let fail_at pos items = fail_kind pos (expect_kind items)

  let source_span st start stop =
    if stop <= start then Span.point (I.span st.input start).start
    else Span.union (I.span st.input start) (I.span st.input (stop - 1))

  (* Running *)

  let default_max_depth = 5000

  let found_at st pos =
    if pos < st.len then I.unexpected st.input pos
    else ("end of input", I.span st.input st.len)

  let expected_message items found =
    match items with
    | [] -> "unexpected " ^ found
    | _ ->
        Printf.sprintf "expected %s, found %s"
          (Expected.list_to_string items)
          found

  let to_diagnostic st e =
    let context =
      List.rev_map
        (fun (name, start) -> (name, source_span st start e.pos))
        e.extra.context
    in
    let expected_diagnostic expected =
      let found, span = found_at st e.pos in
      Diagnostic.make ~expected ~found ~context ~labels:e.extra.labels
        ~notes:e.extra.notes ~span
        (expected_message expected found)
    in
    match e.kind with
    | Expect { items; unexpected } -> (
        match (flatten items, unexpected) with
        | [], Some stop when stop > e.pos ->
            let found = I.describe st.input e.pos stop in
            Diagnostic.make ~found ~context ~labels:e.extra.labels
              ~notes:e.extra.notes
              ~span:(source_span st e.pos stop)
              ("unexpected " ^ found)
        | expected, _ -> expected_diagnostic expected)
    | Custom { code; message } ->
        let span =
          match e.extra.span with
          | Some s -> s
          | None -> Span.point (I.span st.input e.pos).start
        in
        Diagnostic.make ?code ~context ~labels:e.extra.labels
          ~notes:e.extra.notes ~span message

  (* The diagnostic for an error that is being recovered from: the active
     enclosing contexts that started before the error, then the error's own
     frames, as if it had propagated to the top. *)
  let recovered_diagnostic st e =
    let rec enclosing acc = function
      | No_frame -> acc
      | Frame (name, start, rest) ->
          enclosing (if start < e.pos then (name, start) :: acc else acc) rest
    in
    to_diagnostic st
      {
        e with
        extra =
          { e.extra with context = enclosing [] st.frames @ e.extra.context };
      }

  (* The source offset of input index [pos]. *)
  let offset st pos = (I.span st.input (min pos st.len)).start

  (* Recording. Past [max_errors] errors, further errors are dropped (the run
     goes on). A recovery error ([found] is the offset of its failure position)
     is also dropped if the latest recovery error was found at the same offset
     (a cascade); [emit]ted diagnostics ([found = None]) never are. *)
  let record ~found st d =
    if not (Diagnostic.is_error d) then
      st.log <- { diagnostic = d; found } :: st.log
    else if st.errors < st.max_errors then begin
      let rec duplicate f = function
        | [] -> false
        | { found = Some g; _ } :: _ -> f = g
        | { found = None; _ } :: rest -> duplicate f rest
      in
      let dup =
        match found with Some f -> duplicate f st.log | None -> false
      in
      if not dup then begin
        st.log <- { diagnostic = d; found } :: st.log;
        st.errors <- st.errors + 1
      end
    end

  (* Remember that [start, stop) was skipped as trivia. *)
  let note_trivia st start stop =
    st.trivia_start <- start;
    st.trivia_stop <- stop

  let restore st log errors =
    st.log <- log;
    st.errors <- errors

  let run max_depth max_errors p input =
    if max_errors < 1 then invalid_arg "Parkour: max_errors must be positive";
    let st =
      {
        input;
        len = I.length input;
        max_depth;
        max_errors;
        depth = 0;
        log = [];
        errors = 0;
        frames = No_frame;
        trivia_start = 0;
        trivia_stop = 0;
      }
    in
    let value =
      match p st 0 with
      | Pass r -> Some (r.value, r.pos)
      | Fail r ->
          record
            ~found:(Some (offset st r.error.pos))
            st (to_diagnostic st r.error);
          None
      | exception Too_deep pos ->
          let span = Span.point (I.span st.input (min pos st.len)).start in
          record ~found:(Some span.start) st
            (Diagnostic.make ~span
               (Printf.sprintf "nesting too deep (more than %d levels)"
                  max_depth));
          None
    in
    let start d = (Diagnostic.span d).start in
    ( value,
      List.stable_sort
        (fun a b -> compare (start a) (start b))
        (List.rev_map (fun e -> e.diagnostic) st.log) )

  (* Basic combinators *)

  let return x _ pos = pass x pos

  let fail ?code ?span ?(labels = []) ?(notes = []) message _ pos =
    Fail
      {
        error =
          {
            pos;
            kind = Custom { code; message };
            extra = { no_extra with span; labels; notes };
          };
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
                hint = keep_join b.pos a.hint b.hint;
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
                hint = keep_join b.pos a.hint b.hint;
              }
        | Fail b ->
            Fail { error = with_hint a.hint b.error; cut = a.cut || b.cut })

  let both p q = seq p q (fun a b -> (a, b))

  (* Ordered choice. A committed result of either side is final, and a
     committed [q] discards [p]'s failure: once committed, alternatives that
     were not taken are no longer relevant to the report. *)
  let alt p q st pos =
    let log = st.log and errors = st.errors in
    match p st pos with
    | Pass _ as r -> r
    | Fail { cut = true; _ } as r -> r
    | Fail a -> (
        restore st log errors;
        match q st pos with
        | Pass b when not b.cut ->
            Pass
              {
                value = b.value;
                pos = b.pos;
                cut = false;
                hint =
                  (if a.error.pos < b.pos then b.hint
                   else keep_join b.pos (Some a.error) b.hint);
              }
        | Fail b when not b.cut ->
            Fail { error = merge a.error b.error; cut = false }
        | r -> r)

  (* Describing failures *)

  (* [label] describes a failure at its start position by its name, whatever
     the failure said (unless it is a custom message). *)
  let label name p =
    let described = expect_kind [ Expected.Label name ] in
    let relabel e =
      match e.kind with
      | Expect _ -> { e with kind = described }
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
        if stop > pos then note_trivia st pos stop;
        Pass { value; pos = stop; cut; hint = None }
    | Pass { pos = stop; _ } as r ->
        if stop > pos then note_trivia st pos stop;
        r
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
  let add_frame name pos e =
    if e.pos > pos then
      {
        e with
        extra = { e.extra with context = (name, pos) :: e.extra.context };
      }
    else e

  let context name p st pos =
    let frames = st.frames in
    st.frames <- Frame (name, pos, frames);
    let reply = p st pos in
    st.frames <- frames;
    match reply with
    | Pass ({ hint = Some h; _ } as r) ->
        Pass
          {
            value = r.value;
            pos = r.pos;
            cut = r.cut;
            hint = Some (add_frame name pos h);
          }
    | Pass _ as r -> r
    | Fail r -> Fail { error = add_frame name pos r.error; cut = r.cut }

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
                extra = { no_extra with span = Some (source_span st pos r.pos) };
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

  (* Same as folding [alt], but iterative. The failures of the alternatives
     tried so far are kept as a list, newest first, of those at the furthest
     position (all that [merge] would keep); they are merged, oldest first,
     only when the result needs them. A success usually starts past the
     failures, which are then dropped without having been merged. *)
  let merge_all errs =
    match errs with
    | [] -> assert false
    | [ e ] -> e
    | [ later; earlier ] -> merge earlier later
    | _ -> (
        match List.rev errs with
        | [] -> assert false
        | e :: rest -> List.fold_left merge e rest)

  (* [go] is a top-level function, not a closure made on each call. *)
  let rec choice_go st pos log errors furthest errs = function
    | [] ->
        let error =
          match errs with [] -> expect_error pos [] | _ -> merge_all errs
        in
        Fail { error; cut = false }
    | p :: rest -> (
        match p st pos with
        | Pass r when not r.cut -> (
            match errs with
            | [] -> Pass r
            | _ when furthest < r.pos -> Pass r
            | _ ->
                Pass
                  {
                    r with
                    hint = keep_join r.pos (Some (merge_all errs)) r.hint;
                  })
        | Fail r when not r.cut ->
            restore st log errors;
            let e = r.error in
            if e.pos > furthest then
              choice_go st pos log errors e.pos [ e ] rest
            else if e.pos = furthest then
              choice_go st pos log errors furthest (e :: errs) rest
            else choice_go st pos log errors furthest errs rest
        | r -> r)

  let choice ?label:name ps =
    let p st pos = choice_go st pos st.log st.errors (-1) [] ps in
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

  (* [fold_go] is a top-level function, not a closure made on each call. *)
  let rec fold_go name f p st acc pos cut hint =
    let log = st.log and errors = st.errors in
    match p st pos with
    | Pass r ->
        if r.pos = pos then zero_width name pos;
        fold_go name f p st (f acc r.value) r.pos (cut || r.cut)
          (keep_join r.pos hint r.hint)
    | Fail r when r.cut -> Fail { error = with_hint hint r.error; cut = true }
    | Fail r ->
        restore st log errors;
        Pass
          { value = acc; pos; cut; hint = keep pos (join hint (Some r.error)) }

  let fold_loop name f init p st pos0 = fold_go name f p st init pos0 false None
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
        let log = st.log and errors = st.errors in
        match sep st pos with
        | Fail r when r.cut ->
            Fail { error = with_hint hint r.error; cut = true }
        | Fail r ->
            restore st log errors;
            Pass
              {
                value = acc;
                pos;
                cut;
                hint = keep pos (join hint (Some r.error));
              }
        | Pass s -> (
            let cut = cut || s.cut and hint = keep_join s.pos hint s.hint in
            let log = st.log and errors = st.errors in
            match p st s.pos with
            | Pass r ->
                if r.pos = pos then zero_width name pos;
                go (r.value :: acc) r.pos (cut || r.cut)
                  (keep_join r.pos hint r.hint)
            | Fail r when r.cut ->
                Fail { error = with_hint hint r.error; cut = true }
            | Fail r ->
                restore st log errors;
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
                (keep_join r.pos hint r.hint)
          | Fail r ->
              Fail { error = with_hint hint r.error; cut = cut || r.cut }
      in
      go n [] pos0 false None

  (* Each step is a choice between [stop] and [p], so a committed [p] failure
     discards [stop]'s failure, as in [alt]. *)
  let many_till p stop st pos0 =
    let rec go acc pos cut hint =
      let log = st.log and errors = st.errors in
      match stop st pos with
      | Pass s ->
          Pass
            {
              value = List.rev acc;
              pos = s.pos;
              cut = cut || s.cut;
              hint = keep_join s.pos hint s.hint;
            }
      | Fail s when s.cut -> Fail { error = with_hint hint s.error; cut = true }
      | Fail s -> (
          restore st log errors;
          match p st pos with
          | Pass r ->
              if r.pos = pos then zero_width "many_till" pos;
              let hint = if r.cut then hint else join hint (Some s.error) in
              go (r.value :: acc) r.pos (cut || r.cut)
                (keep_join r.pos hint r.hint)
          | Fail r when r.cut ->
              Fail { error = with_hint hint r.error; cut = true }
          | Fail r ->
              Fail { error = with_hint hint (merge s.error r.error); cut })
    in
    go [] pos0 false None

  let optional p = alt (map Option.some p) (return None)
  let option default p = alt p (return default)

  (* Lookahead and position *)

  let end_expected = expect_kind [ Expected.End_of_input ]

  let eof st pos =
    if pos >= st.len then pass () pos else fail_kind pos end_expected

  let lookahead p st pos =
    let log = st.log and errors = st.errors in
    let reply = p st pos in
    restore st log errors;
    match reply with
    | Pass r -> pass r.value pos
    | Fail r -> Fail { r with cut = false }

  let not_followed_by p st pos =
    let log = st.log and errors = st.errors in
    let reply = p st pos in
    restore st log errors;
    match reply with
    | Fail _ -> pass () pos
    | Pass r ->
        Fail
          {
            error =
              {
                pos;
                kind = Expect { items = Items []; unexpected = Some r.pos };
                extra = no_extra;
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

  let expect name f =
    let kind = expect_kind [ Expected.Label name ] in
    fun st pos ->
      if pos < st.len then
        match f (I.get st.input pos) with
        | Some v -> pass v (pos + 1)
        | None -> fail_kind pos kind
      else fail_kind pos kind

  let satisfy name pred =
    let kind = expect_kind [ Expected.Label name ] in
    fun st pos ->
      if pos < st.len && pred (I.get st.input pos) then
        pass (I.get st.input pos) (pos + 1)
      else fail_kind pos kind

  let token t =
    let kind = expect_kind [ Expected.Literal (I.show t) ] in
    fun st pos ->
      if pos < st.len && I.equal (I.get st.input pos) t then pass t (pos + 1)
      else fail_kind pos kind

  (* [label name (peek >>= ...)] without the bind: nothing is committed or
     hinted by looking at the token, so the chosen parser's reply is the reply. *)
  let dispatch ~label:name f =
    label name (fun st pos ->
        if pos < st.len then
          match f (I.get st.input pos) with
          | Some p -> p st pos
          | None -> fail_kind pos nothing_expected
        else fail_kind pos nothing_expected)

  let any st pos =
    if pos < st.len then pass (I.get st.input pos) (pos + 1)
    else fail_kind pos nothing_expected

  let peek st pos =
    pass (if pos < st.len then Some (I.get st.input pos) else None) pos

  (* Operators and grouping (PLAN.md §3.7). Parsing is plain combinators, so
     commitment, hints and merging are inherited; the parsed sequence is then
     grouped by an iterative shunting-yard pass. *)

  type 'a operator =
    | Prefix of ('a -> 'a) t
    | Postfix of ('a -> 'a) t
    | Infix_left of ('a -> 'a -> 'a) t
    | Infix_right of ('a -> 'a -> 'a) t
    | Infix_none of ('a -> 'a -> 'a) t

  (* Prefix operators (outermost first, with their right binding powers) and
     the term they apply to. *)
  type 'a operand = (int * ('a -> 'a)) list * 'a

  type 'a segment =
    | Post_seg of int * ('a -> 'a)  (** left binding power, function *)
    | Infix_seg of {
        lbp : int;
        rbp : int;
        none : bool;
        f : 'a -> 'a -> 'a;
        span : Span.t;
        operand : 'a operand;
      }

  type 'a pending =
    | Pending_prefix of int * ('a -> 'a)
    | Pending_infix of int * bool * ('a -> 'a -> 'a)

  (* [Error span]: a non-associative operator chained, [span] being the
     second operator. *)
  let group ((pres, v) : 'a operand) (segments : 'a segment list) =
    let push_operand (ops, vals) (pres, v) =
      ( List.fold_left (fun ops (r, f) -> Pending_prefix (r, f) :: ops) ops pres,
        v :: vals )
    in
    (* Reduce the stacked operators that bind at least as tightly as the
       incoming one, whose left binding power is [l]. *)
    let rec reduce incoming l (ops, vals) =
      match (ops, vals) with
      | Pending_prefix (r, f) :: ops, v :: vals when r >= l ->
          reduce incoming l (ops, f v :: vals)
      | Pending_infix (r, none, f) :: ops, b :: a :: vals when r >= l -> (
          match incoming with
          | Some span when none && r = l -> Error span
          | _ -> reduce incoming l (ops, f a b :: vals))
      | _ -> Ok (ops, vals)
    in
    let ( let* ) = Result.bind in
    let rec go stack = function
      | [] -> (
          let* _, vals = reduce None min_int stack in
          match vals with [ v ] -> Ok v | _ -> assert false)
      | Post_seg (l, f) :: rest -> (
          let* ops, vals = reduce None l stack in
          match vals with
          | v :: vals -> go (ops, f v :: vals) rest
          | [] -> assert false)
      | Infix_seg s :: rest ->
          let* ops, vals = reduce (Some s.span) s.lbp stack in
          go
            (push_operand
               (Pending_infix (s.rbp, s.none, s.f) :: ops, vals)
               s.operand)
            rest
    in
    go (push_operand ([], []) (pres, v)) segments

  let chained_message = "operator cannot be chained"

  let expression term table =
    let levels = List.filter (fun l -> l <> []) table in
    let n = List.length levels in
    let associativity = function
      | Infix_left _ -> Some 0
      | Infix_right _ -> Some 1
      | Infix_none _ -> Some 2
      | Prefix _ | Postfix _ -> None
    in
    List.iter
      (fun level ->
        match List.filter_map associativity level with
        | a :: rest when List.exists (fun b -> a <> b) rest ->
            invalid_arg
              "Parkour.expression: a level mixes infix associativities"
        | _ -> ())
      levels;
    (* Strength [i = n - k] for the k-th level; see PLAN.md §3.7. *)
    let flat =
      List.concat
        (List.mapi
           (fun k level -> List.map (fun op -> (n - k, op)) level)
           levels)
    in
    let prefix_ps =
      List.filter_map
        (function
          | i, Prefix p -> Some (map (fun f -> ((6 * i) + 2, f)) p) | _ -> None)
        flat
    in
    let operand =
      seq (many (choice prefix_ps)) term (fun pres v -> ((pres, v) : _ operand))
    in
    let infix i rbp none p =
      seq (spanned p) operand (fun (f, span) operand ->
          Infix_seg { lbp = 6 * i; rbp; none; f; span; operand })
    in
    let operator_ps =
      List.filter_map
        (fun (i, op) ->
          match op with
          | Prefix _ -> None
          | Postfix p -> Some (map (fun f -> Post_seg ((6 * i) + 4, f)) p)
          | Infix_left p -> Some (infix i (6 * i) false p)
          | Infix_right p -> Some (infix i ((6 * i) - 1) false p)
          | Infix_none p -> Some (infix i (6 * i) true p))
        flat
    in
    let sequence = seq operand (many (choice operator_ps)) group in
    fun st pos ->
      match sequence st pos with
      | Fail _ as r -> r
      | Pass ({ value = Ok v; _ } as r) -> Pass { r with value = v }
      | Pass { value = Error span; pos = stop; cut; hint } ->
          let e =
            {
              pos = stop;
              kind = Custom { code = None; message = chained_message };
              extra = { no_extra with span = Some span };
            }
          in
          Fail { error = with_hint hint e; cut }

  (* [seq (seq o p) c], except that a failure of [c] that is the reported
     one points back at the opener. *)
  let chainl1 p op =
    let step = seq op p (fun f x -> (f, x)) in
    bind p (fun x -> fold_loop "chainl1" (fun acc (f, y) -> f acc y) x step)

  let chainr1 p op =
    let step = seq op p (fun f x -> (f, x)) in
    (* [rev_steps] is [(f_n, a_n); ...; (f_1, a_1)]: a0 f1 (a1 f2 (... an)). *)
    let fold_right a0 = function
      | [] -> a0
      | (fn, an) :: rest ->
          let rec go r f = function
            | [] -> f a0 r
            | (f', a) :: rest -> go (f a r) f' rest
          in
          go an fn rest
    in
    bind p (fun a0 -> map (fold_right a0) (fold_loop "chainr1" cons [] step))

  (* Error recovery (PLAN.md §5) *)

  type catch = [ `Committed | `Any ]

  let recover ?(catch = `Committed) ~sync ~fallback p st start =
    match p st start with
    | Pass _ as r -> r
    | Fail { cut = false; _ } as r when catch = `Committed -> r
    | Fail r as failed -> (
        let log = st.log and errors = st.errors in
        (* Recorded first, so that whatever [sync] logs cannot hide it. *)
        record
          ~found:(Some (offset st r.error.pos))
          st
          (recovered_diagnostic st r.error);
        match sync st r.error.pos with
        | Pass s when s.pos > start ->
            Pass
              {
                value = fallback (source_span st start s.pos);
                pos = s.pos;
                cut = r.cut;
                hint = None;
              }
        | _ ->
            restore st log errors;
            failed)

  (* The diagnostic for a token that is missing at [e.pos]: an empty span at
     the insertion point (just after the previous token, before any trivia),
     and the token found there as an unlabelled secondary span. *)
  let insertion_diagnostic st e extra_labels =
    let d = recovered_diagnostic st e in
    let at =
      if st.trivia_stop = e.pos && st.trivia_start < e.pos then
        (I.span st.input st.trivia_start).start
      else if e.pos > 0 then (I.span st.input (e.pos - 1)).stop
      else (I.span st.input e.pos).start
    in
    let found_span = Diagnostic.span d in
    let labels =
      Diagnostic.labels d
      @ (if Span.is_empty found_span || found_span.start = at then []
         else [ (found_span, "") ])
      @ extra_labels
    in
    Diagnostic.make
      ~severity:(if Diagnostic.is_error d then `Error else `Warning)
      ?code:(Diagnostic.code d) ~expected:(Diagnostic.expected d)
      ?found:(Diagnostic.found d) ~context:(Diagnostic.context d)
      ?label:(Diagnostic.label d) ~labels ~notes:(Diagnostic.notes d)
      ~span:(Span.point at) (Diagnostic.message d)

  let or_insert default p st pos =
    let log = st.log and errors = st.errors in
    match p st pos with
    | Fail { error; cut = false } when error.pos = pos ->
        restore st log errors;
        record
          ~found:(Some (offset st error.pos))
          st
          (insertion_diagnostic st error []);
        pass default pos
    | r -> r

  (* The "opened here" label of a delimited construct that opened at [pos]
     and whose opener ended at [stop], if its body consumed anything. *)
  let opened st pos stop body_stop =
    if stop > pos && body_stop > stop then
      [ (source_span st pos stop, "opened here") ]
    else []

  let between ?(or_insert = false) o c p st pos =
    match o st pos with
    | Fail _ as r -> r
    | Pass a -> (
        match p st a.pos with
        | Fail b ->
            Fail { error = with_hint a.hint b.error; cut = a.cut || b.cut }
        | Pass b -> (
            let cut = a.cut || b.cut in
            let hint = keep_join b.pos a.hint b.hint in
            let log = st.log and errors = st.errors in
            match c st b.pos with
            | Pass d ->
                Pass
                  {
                    value = b.value;
                    pos = d.pos;
                    cut = cut || d.cut;
                    hint = keep_join d.pos hint d.hint;
                  }
            | Fail { error; cut = false } when or_insert && error.pos = b.pos ->
                restore st log errors;
                record
                  ~found:(Some (offset st error.pos))
                  st
                  (insertion_diagnostic st error (opened st pos a.pos b.pos));
                Pass { value = b.value; pos = b.pos; cut; hint = None }
            | Fail d ->
                let error = with_hint hint d.error in
                let error =
                  if d.error.pos >= error.pos then
                    {
                      error with
                      extra =
                        {
                          error.extra with
                          labels =
                            error.extra.labels @ opened st pos a.pos b.pos;
                        };
                    }
                  else error
                in
                Fail { error; cut = cut || d.cut }))

  let emit d st pos =
    record ~found:None st d;
    pass () pos

  (* [past]: consume the match of [stop]. Probes run [q] at [pos] and answer
     the end of its match, or -1; they leave no diagnostics. *)
  let skip ~past ?nested stop st pos0 =
    let probe q pos =
      let log = st.log and errors = st.errors in
      let e = match q st pos with Pass r -> r.pos | Fail _ -> -1 in
      restore st log errors;
      e
    in
    let rec go pos depth =
      let e = if depth = 0 then probe stop pos else -1 in
      if e >= 0 then pass () (if past then e else pos)
      else if pos >= st.len then pass () pos
      else
        match nested with
        | None -> go (pos + 1) 0
        | Some (opener, closer) ->
            (* Inside, [closer] first, so that identical delimiters work. *)
            let c = if depth > 0 then probe closer pos else -1 in
            if c >= 0 then go (max c (pos + 1)) (depth - 1)
            else
              let o = probe opener pos in
              if o >= 0 then go (max o (pos + 1)) (depth + 1)
              else if depth = 0 && probe closer pos >= 0 then pass () pos
              else go (pos + 1) depth
    in
    go pos0 0

  let skip_to ?nested stop = skip ~past:false ?nested stop
  let skip_past ?nested stop = skip ~past:true ?nested stop

  (* Running *)

  let parse_prefix ?(max_depth = default_max_depth) ?(max_errors = 100) p input
      =
    match run max_depth max_errors p input with
    | Some (x, pos), log when not (List.exists Diagnostic.is_error log) ->
        Ok (x, pos)
    | _, log -> Error log

  let parse_partial ?(max_depth = default_max_depth) ?(max_errors = 100) p input
      =
    let value, log =
      run max_depth max_errors (seq p eof (fun x () -> x)) input
    in
    (Option.map fst value, log)

  let parse ?max_depth ?max_errors p input =
    match parse_partial ?max_depth ?max_errors p input with
    | Some x, log when not (List.exists Diagnostic.is_error log) -> Ok x
    | _, log -> Error log
end
