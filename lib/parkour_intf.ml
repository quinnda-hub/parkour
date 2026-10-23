(** Module types shared by every Parkour parser instance. *)

exception Grammar_error of string
(** Raised (never returned) when a grammar is broken regardless of input, e.g.
    {!S.many} applied to a parser that succeeds without consuming input, which
    would otherwise loop forever. The message names the combinator and the input
    position. *)

(** What the parsing engine needs to know about its input. *)
module type INPUT = sig
  type t
  (** A complete input, e.g. a string or an array of lexed tokens. *)

  type token
  (** One unit of input, e.g. [char]. *)

  val length : t -> int
  val get : t -> int -> token
  val equal : token -> token -> bool

  val show : token -> string
  (** Source text of a token, used for {!Expected.Literal}. *)

  val span : t -> int -> Span.t
  (** [span input i] is the source span of the token at index [i]. For
      [i = length input] it is the empty span just past the last token. *)

  val unexpected : t -> int -> string * Span.t
  (** [unexpected input i], for [i < length input], describes the input at [i]
      for "found ..." messages (already quoted, e.g. [{|"banana"|}] or
      [newline]) along with the source span to underline. *)

  val describe : t -> int -> int -> string
  (** [describe input start stop], for [start < stop <= length input], describes
      the tokens from [start] up to (excluding) [stop], already quoted, for
      "unexpected ..." messages, e.g. what {!S.not_followed_by}'s parser
      matched. *)
end

(** What {!Parkour.Tokens} needs to know about a token type. *)
module type TOKEN = sig
  type t

  val equal : t -> t -> bool

  val show : t -> string
  (** How the token is written in the source, e.g. [";"] or [let]. Used for
      {!Expected.Literal}, so [expected ';'] names the token. *)
end

(** The combinator interface, identical for every input type.

    {2 Semantics}

    {b Backtracking by default.} [p <|> q] runs [p]; if it fails, [q] runs from
    the same position, however much input [p] consumed. Choice is ordered: the
    first alternative that succeeds wins, and a later failure never revisits it.

    {b Commitment with {!cut}.} Once a parser passes a [cut], the enclosing
    alternatives, repetitions and options can no longer backtrack past it: a
    failure after the cut is final and is reported as is. Use cuts once enough
    input has been seen to know which construct is intended (after a keyword,
    after [=]). {!attempt} undoes commitment for the parser it wraps.

    {b Error reporting.} When parsing fails, Parkour reports the failure that
    got furthest into the input. Failures at the same position are combined:
    [expected "let" or "type", found "lte"]. Failures of parsers that were
    abandoned in favour of a successful alternative are still considered, so a
    repetition that stopped early contributes what it would have accepted:
    [expected digit or ';', found 'x']. {!label} and {!context} control how
    failures are described.

    {b Termination and depth.} Repetitions are loops, so long inputs do not grow
    the stack. Each entry into a recursive parser ({!fix}, {!delay}, {!force})
    counts towards [max_depth]; exceeding it ends the parse with a
    [nesting too deep] error rather than a stack overflow. *)
module type S = sig
  type input
  type token

  type 'a t
  (** A parser producing ['a]. Parsers are immutable; build them once and run
      them many times. *)

  (** {1 Running} *)

  val default_max_depth : int
  (** [5000]. *)

  val parse :
    ?max_depth:int ->
    ?max_errors:int ->
    'a t ->
    input ->
    ('a, Diagnostic.t list) result
  (** [parse p input] runs [p] and requires it to consume the whole input. On
      failure the list is non-empty and ordered by position.

      Without {{!recovery} error recovery} there is exactly one diagnostic. With
      it, [Ok] is returned only if no error was recorded: recovering from an
      error still makes [parse] return [Error], with every recorded diagnostic
      (warnings included). Warnings alone do not make [parse] fail, but they are
      dropped on [Ok]: use {!parse_partial} to see them.

      [max_errors] (default [100], must be positive) caps the errors recorded:
      once that many are recorded, further ones are dropped and parsing goes on
      to the end as usual. *)

  val parse_partial :
    ?max_depth:int ->
    ?max_errors:int ->
    'a t ->
    input ->
    'a option * Diagnostic.t list
  (** Like {!parse}, for tools that want a result despite errors. The value is
      [Some v] if [p] (followed by end of input) finished, possibly after
      recovering from errors, and [None] if it failed without recovering or the
      nesting limit was exceeded. The diagnostics are everything recorded,
      including warnings and the final failure, sorted by the start of their
      span. After a [nesting too deep] abort they may include diagnostics of
      branches that were still in progress and would have been abandoned. *)

  val parse_prefix :
    ?max_depth:int ->
    ?max_errors:int ->
    'a t ->
    input ->
    ('a * int, Diagnostic.t list) result
  (** Like {!parse} but stops wherever [p] stops, also returning the input index
      reached. *)

  (** {1 Basic combinators} *)

  val return : 'a -> 'a t

  val fail :
    ?code:string ->
    ?span:Span.t ->
    ?labels:(Span.t * string) list ->
    ?notes:string list ->
    string ->
    'a t
  (** [fail msg] fails at the current position with a custom message. A custom
      message takes precedence over plain expectations at the same position.
      Does not commit.

      [span] is what the diagnostic underlines instead of the current position
      (the failure is still at the current position, which is what decides
      between failures). [labels] and [notes] end up in the diagnostic (see
      {!Diagnostic.make}): secondary spans and hints. Spans can be had from
      {!spanned}, so a check after parsing can point at both places:
      {[
      let* name, span = spanned identifier in
      if Hashtbl.mem seen name then
        fail ~span
          ~labels:[ (Hashtbl.find seen name, "first defined here") ]
          "duplicate key"
      else return name
      ]} *)

  val map : ('a -> 'b) -> 'a t -> 'b t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val both : 'a t -> 'b t -> ('a * 'b) t
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
  val ( >>| ) : 'a t -> ('a -> 'b) -> 'b t
  val ( <$> ) : ('a -> 'b) -> 'a t -> 'b t
  val ( <*> ) : ('a -> 'b) t -> 'a t -> 'b t
  val ( *> ) : _ t -> 'a t -> 'a t
  val ( <* ) : 'a t -> _ t -> 'a t
  val ( <|> ) : 'a t -> 'a t -> 'a t
  val ( <?> ) : 'a t -> string -> 'a t
  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
  val ( and+ ) : 'a t -> 'b t -> ('a * 'b) t

  (** Just the operators, for [let open P.Syntax in]. *)
  module Syntax : sig
    val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
    val ( >>| ) : 'a t -> ('a -> 'b) -> 'b t
    val ( <$> ) : ('a -> 'b) -> 'a t -> 'b t
    val ( <*> ) : ('a -> 'b) t -> 'a t -> 'b t
    val ( *> ) : _ t -> 'a t -> 'a t
    val ( <* ) : 'a t -> _ t -> 'a t
    val ( <|> ) : 'a t -> 'a t -> 'a t
    val ( <?> ) : 'a t -> string -> 'a t
    val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
    val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
    val ( and+ ) : 'a t -> 'b t -> ('a * 'b) t
  end

  (** {1 Choice and commitment} *)

  val choice : ?label:string -> 'a t list -> 'a t
  (** [choice [p1; ...; pn]] is [p1 <|> ... <|> pn], labelled if [label] is
      given. [choice []] always fails. *)

  val dispatch : label:string -> (token -> 'a t option) -> 'a t
  (** [dispatch ~label f] picks the parser from the next token without trying
      alternatives: if [f t] is [Some p] for the next token [t], it runs [p]; if
      it is [None], or the input has ended, it fails at the current position
      with no expectations. The whole is labelled [label] (see {!label}), so
      that failure reads "expected <label>, found ...". Nothing is consumed by
      looking at the token.

      {[
      let value =
        dispatch ~label:"value" (function
          | '"' -> Some string_literal
          | '[' -> Some array
          | '{' -> Some object_
          | '-' | '0' .. '9' -> Some number
          | _ -> None)
      ]}

      It is a fast path for {!choice} over many alternatives. When the
      alternatives' first tokens are disjoint, each fails at its start position
      on tokens outside its own, and each consumes input when it succeeds,
      [choice ~label [p1; ...; pn]] and [dispatch ~label f] (with [f] sending
      the first tokens of [pi] to [pi]) succeed and fail identically, with the
      same diagnostics. *)

  val cut : unit t
  (** Consumes nothing; commits the current branch (see the semantics above). *)

  val attempt : 'a t -> 'a t
  (** [attempt p] behaves like [p] but any commitment made inside [p] does not
      escape it: enclosing alternatives may backtrack over [p] again. *)

  (** {1 Repetition}

      In the open-ended repetitions ([many], [sep_by], [fold_many] and their
      variants), an iteration that fails {e without} committing ends the
      repetition (the input it consumed is given back), and an iteration that
      fails after a {!cut} fails the whole repetition. They raise
      [Grammar_error] if an iteration succeeds without consuming input, since it
      would repeat forever. {!count} and {!many_till} differ; see each. *)

  val many : 'a t -> 'a list t
  val many1 : 'a t -> 'a list t

  val sep_by : sep:_ t -> 'a t -> 'a list t
  (** [sep_by ~sep p] parses zero or more [p] separated by [sep]. A trailing
      [sep] is not consumed. *)

  val sep_by1 : sep:_ t -> 'a t -> 'a list t

  val sep_end_by : sep:_ t -> 'a t -> 'a list t
  (** Like {!sep_by} but allows (and consumes) one trailing separator. *)

  val count : int -> 'a t -> 'a list t
  (** [count n p]: exactly [n] times; any failure of [p] fails [count]. Never
      raises [Grammar_error]. Raises [Invalid_argument] (when built) if [n < 0].
  *)

  val many_till : 'a t -> _ t -> 'a list t
  (** [many_till p stop] tries [stop], then [p], repeatedly, until [stop]
      succeeds (consuming it). Fails if neither succeeds, reporting what both
      expected. Raises [Grammar_error] if [p] succeeds without consuming input.
  *)

  val skip_many : _ t -> unit t
  val skip_many1 : _ t -> unit t

  val fold_many : ('acc -> 'a -> 'acc) -> 'acc -> 'a t -> 'acc t
  (** {!many} without building a list. *)

  val optional : 'a t -> 'a option t
  (** [None] if [p] fails without committing. *)

  val option : 'a -> 'a t -> 'a t
  (** [option default p]. *)

  (** {1 Lookahead and position} *)

  val eof : unit t
  (** Succeeds only at the end of input. Expects [end of input]. *)

  val lookahead : 'a t -> 'a t
  (** Runs [p] without consuming input or committing. *)

  val not_followed_by : _ t -> unit t
  (** Succeeds, consuming nothing, iff [p] fails here. Otherwise fails with
      [unexpected X], where X is the input [p] matched: e.g.
      [not_followed_by (string "let")] on ["letter"] reports [unexpected "let"].
      Never commits. *)

  val position : int t
  (** Current input index (for text: byte offset). *)

  val spanned : 'a t -> ('a * Span.t) t
  (** Also returns the source span [p] consumed. *)

  (** {1 Describing failures} *)

  val label : string -> 'a t -> 'a t
  (** [label name p]: if [p] fails without consuming input, report
      [expected name] instead of [p]'s own expectations. Failures deeper inside
      [p] are reported unchanged, since they are more precise. This also applies
      when [p] succeeds without consuming input: in
      [label "items" (many item) *> char ';'] on ["x"], the error is
      [expected items or ';']. Custom messages from {!fail} are never replaced.
  *)

  val hidden : 'a t -> 'a t
  (** [hidden p] keeps [p] out of [expected ...] lists, e.g. for optional
      whitespace: when [p] succeeds it adds nothing to later errors, and when it
      fails without consuming input it expects nothing. Failures deeper inside
      [p] (say, an unterminated comment) are reported unchanged. *)

  val context : string -> 'a t -> 'a t
  (** [context "assignment" p]: if [p] fails after its first token, the
      diagnostic records that an assignment was being parsed, and where it
      started. (A failure right at the start is described by {!label} instead.)
  *)

  val validate : ?code:string -> ('a -> ('b, string) result) -> 'a t -> 'b t
  (** [validate check p] runs [p], then [check] on its result. [Error msg]
      becomes a failure whose span covers everything [p] consumed, e.g.
      [validate check_range integer] underlines the whole out-of-range number.
      Does not commit. *)

  (** {1 Recursion}

      Use {!fix} for a single recursive parser, and [let rec ... = lazy ...]
      with {!force} for mutually recursive ones:
      {[
      let rec expr = lazy (sum_of (force term))
      and term = lazy (number <|> parens (force expr))
      ]}
      All three count towards [max_depth] each time they are entered.

      A [lazy] parser is forced the first time it runs; force it once (or run it
      once) before using it from several domains at the same time. {!fix} does
      this itself. *)

  val fix : ('a t -> 'a t) -> 'a t
  val force : 'a t Lazy.t -> 'a t

  val delay : (unit -> 'a t) -> 'a t
  (** [delay f] calls [f ()] each time the parser runs. *)

  (** {1 Operators and grouping}

      {!expression} parses operator expressions from a table of operators. The
      table lists the levels from the tightest-binding to the loosest, and each
      level mixes any {!operator}s, e.g.
      {[
      expression term
        [
          [ Infix_right pow ];
          [ Prefix neg ];
          [ Infix_left mul; Infix_left div ];
          [ Infix_left add; Infix_left sub ];
          [ Infix_none lt; Infix_none eq ];
        ]
      ]}

      Rules:
      - Tighter levels bind tighter. Within a level, a postfix operator binds
        tighter than a prefix one, which binds tighter than an infix one: with
        [-] prefix and [!] postfix on one level, [-x!] is [-(x!)]; with [-]
        prefix and [+] infix, [-a+b] is [(-a)+b].
      - A prefix operator may start any operand, even of a tighter operator:
        [2 ^ -1] is [2 ^ (-1)]. Its own operand extends as far as its level
        allows: in the table above [-2 ^ 2] is [-(2 ^ 2)] because [^] is tighter
        than the prefix [-], and [-2 * 3] is [(-2) * 3].
      - Prefix and postfix operators may repeat ([- - x], [x! !]).
      - Left-associative operators group [a - b - c] as [(a - b) - c],
        right-associative ones as [a ^ (b ^ c)]. Non-associative ones reject
        [a < b < c], reporting [operator cannot be chained] and underlining the
        second operator.
      - A level may not mix {!Infix_left}, {!Infix_right} and {!Infix_none}.

      Errors list every operator that could continue the expression; use
      {!label} on operator parsers to shorten that. Operators are tried in table
      order, so one that is a prefix of another (["<"] and ["<="]) must come
      after it, or be written to fail on the longer one. Long chains of
      operators do not consume stack.

      Put a {!cut} in operator parsers (after the operator's token): a missing
      operand is then reported at the operand
      ([expected expression, found ';']). Without it the operator is simply not
      consumed, the expression ends before it, and the error shows up later at
      the operator ([expected ';', found '-']). *)

  type 'a operator =
    | Prefix of ('a -> 'a) t
    | Postfix of ('a -> 'a) t
    | Infix_left of ('a -> 'a -> 'a) t
    | Infix_right of ('a -> 'a -> 'a) t
    | Infix_none of ('a -> 'a -> 'a) t
        (** An operator and the function it applies. Parse the operator's token
            (with trailing whitespace) and return the function. *)

  val expression : 'a t -> 'a operator list list -> 'a t
  (** [expression term table] parses [term]s combined with the table's
      operators, tightest level first. Raises [Invalid_argument] immediately if
      a level mixes infix associativities. Empty levels are ignored.

      It is built from {!many}, {!choice} and sequencing: an operand is any
      number of prefix operators then [term]; after an operand, postfix and
      infix operators (each infix followed by an operand) are tried in table
      order. Hence commitment, backtracking and error merging behave exactly as
      for those combinators. An infix operator whose operand fails is not
      consumed, so a [term] must not accept a tail that is also an operator.

      The [operator cannot be chained] failure is reported where the expression
      ends, like {!validate}, and does not commit. *)

  val chainl1 : 'a t -> ('a -> 'a -> 'a) t -> 'a t
  (** [chainl1 p op] parses [p] separated by [op], folding to the left:
      [a - b - c] is [(a - b) - c]. Same as [expression p [[Infix_left op]]]. *)

  val chainr1 : 'a t -> ('a -> 'a -> 'a) t -> 'a t
  (** Like {!chainl1}, folding to the right: [a ^ b ^ c] is [a ^ (b ^ c)]. *)

  val between : ?or_insert:bool -> _ t -> _ t -> 'a t -> 'a t
  (** [between open_ close p] is [open_ *> p <* close]. When [close] fails and
      its failure is the one reported and [p] consumed input, the diagnostic
      gets a secondary label ["opened here"] on the input [open_] consumed, so
      an unclosed bracket is shown together with where it was opened. (Right
      after the opener, the caret already shows it.)

      With [~or_insert:true] (default [false]), if [close] fails without
      committing right where [p] stopped, the failure is recorded as by
      {!or_insert} (at the insertion point, with the token found there
      underlined) together with the ["opened here"] label, and [between]
      succeeds as if [close] had been present. The parse then reports the
      problem but carries on. *)

  (** {1:recovery Error recovery}

      Recovery is opt-in: without it, parsing stops at the first error. A parser
      that recovers records the error as a diagnostic, skips ahead to a
      synchronisation point, substitutes a placeholder and carries on, so one
      run reports many problems and still produces a (partial) result; see
      {!parse_partial}. For example, a file of [name = value;] declarations
      where a broken line is skipped:
      {[
      open (val Lexeme.make ())

      let decl =
        let+ name = identifier
        and+ () = symbol "=" *> cut
        and+ v = integer <* symbol ";" in
        Some (name, v)

      let file = many (recover ~sync:skip_line ~fallback:(fun _ -> None) decl)
      ]}
      On ["a = 1;\nb = ;\nc = 3;\n"], [parse_partial file] gives
      [Some [Some ("a", 1); None; Some ("c", 3)]] and one diagnostic, for line
      two.

      Diagnostics are recorded transactionally: when a branch is abandoned (the
      left side of [<|>] failed, a repetition stopped, a {!lookahead} or
      {!not_followed_by} finished) whatever it recorded is forgotten. A recorded
      diagnostic has the message and {!context} frames the top level would
      report for that error. (Labels and {!hidden} enclosing the {!recover}, and
      alternatives still pending outside it, are not reflected.) A recovery
      error is dropped, as a likely cascade, if the previous recovery error was
      found at the same position of the input (where the unexpected input is,
      not where the diagnostic points: an {!or_insert} points at the end of the
      previous token but was found at the next one); diagnostics from {!emit}
      never are.

      Guidance:
      - Put {!recover} around items that have a {!cut} after their first token,
        so that only malformed items (not merely other alternatives) are
        recovered from.
      - Use [~catch:`Any] only where no alternative should run, such as
        top-level items.
      - Nested blocks recover best with a {!recover} at each level: the
        synchronisation parser starts at the error's position, which may be deep
        inside the block.
      - The synchronisation parser starts {e at the error}, not at the start of
        the item. If the error is a missing terminator just before the next
        item, it can therefore skip that following item too; use {!or_insert}
        for such terminators instead. *)

  type catch = [ `Committed | `Any ]
  (** Which failures {!recover} handles: those after a {!cut}, or all. *)

  val recover :
    ?catch:catch -> sync:_ t -> fallback:(Span.t -> 'a) -> 'a t -> 'a t
  (** [recover ~sync ~fallback p] behaves like [p] when it succeeds. If [p]
      fails (by default only after committing, so that alternatives still run
      for other failures), the error is recorded as a diagnostic, [sync] runs
      starting where the error was found, and the result is [fallback span]
      where [span] covers the input from the start of [p] to where [sync]
      stopped. The commitment of the failure is kept.

      If [sync] fails, or the whole recovery consumes nothing, [p]'s failure is
      returned unchanged (and not recorded) so that an outer [recover] can try;
      this guarantees progress. Exceptions are not caught. *)

  val skip_to : ?nested:_ t * _ t -> _ t -> unit t
  (** [skip_to stop] skips tokens until [stop] would succeed here (without
      consuming it) or the input ends. Never fails. With
      [~nested:(open_, close)], opening and closing delimiters are tracked and
      [stop] is only tried outside them; an unmatched [close] also ends the
      skip, just before it. [open_] and [close] may be the same parser (a quote,
      say), which then toggles inside. Skips one token at a time (one byte for
      text). *)

  val skip_past : ?nested:_ t * _ t -> _ t -> unit t
  (** Like {!skip_to} but consumes the match of [stop]. *)

  val or_insert : 'a -> 'a t -> 'a t
  (** [or_insert v p] is [p] if it succeeds. If [p] fails without committing and
      without consuming input, the failure is recorded (e.g. [expected ';'] for
      a missing semicolon) and [v] is the result, consuming nothing. Other
      failures are unchanged.

      The diagnostic points at the {e insertion point}, where the missing token
      belongs: an empty span right after the previous token, before any
      whitespace or comments (those skipped by {!hidden} parsers or [Lexeme]).
      The token found instead is underlined as a secondary label with no text,
      unless it starts at the insertion point. Inside {!many}, make sure
      something else is consumed on each iteration. *)

  val emit : Diagnostic.t -> unit t
  (** Records a diagnostic and continues, consuming nothing. A warning does not
      make {!parse} fail; an error does. Errors count towards [max_errors]. An
      emitted diagnostic is never dropped as a duplicate. *)

  (** {1 Tokens} *)

  val any : token t
  (** Any single token. Fails only at end of input. *)

  val token : token -> token t
  (** Exactly this token; expects it as a {!Expected.Literal}. *)

  val satisfy : string -> (token -> bool) -> token t
  (** [satisfy name pred]: a token satisfying [pred], expecting [name]. *)

  val expect : string -> (token -> 'a option) -> 'a t
  (** [expect name f]: a token for which [f] returns [Some v]; useful for token
      streams, e.g.
      [expect "identifier" (function Ident s -> Some s | _ -> None)]. *)

  val peek : token option t
  (** Next token without consuming it; [None] at end of input. *)
end

(** The default instance over strings, with byte-level text primitives. *)
module type TEXT = sig
  include S with type input = string and type token = char

  val tokenize :
    ?skip:unit t ->
    'tok t ->
    string ->
    (('tok * Span.t) array, Diagnostic.t list) result
  (** [tokenize ~skip tok text] lexes [text] into an array of tokens with their
      source spans, ready for {!Parkour.Tokens}. It is
      [skip *> many (spanned tok <* skip) <* eof]: [skip] (default: nothing)
      runs before the first token and after each one, so it is where whitespace
      and comments go; [skip] and [tok] are ordinary parsers, so a lexer is
      written like any other grammar. Anything neither of them accepts is
      reported as a diagnostic on [text]:
      {[
      let number = map (fun n -> `Num n) natural
      let plus = map (fun _ -> `Plus) (char '+')
      let lexer = tokenize ~skip:(skip_while is_space) (number <|> plus)
      (* lexer "12 + 3" = Ok [| (`Num 12, 0-2); (`Plus, 3-4); (`Num 3, 5-6) |]
         lexer "12 ? 3" = Error [d], where d's span starts at the '?' *)
      ]} *)

  val char : char -> char t
  (** Same as [token]. *)

  val any_char : char t

  val string : string -> string t
  (** Matches the literal or fails at its start, consuming nothing, with
      [expected "let"]. So [string "let" <|> string "list"] on ["lisx"] reports
      [expected "let" or "list", found "lisx"]. *)

  val take_while : ?label:string -> (char -> bool) -> string t
  (** Longest (possibly empty) prefix satisfying the predicate. If [label] is
      given, it is reported as an alternative when the next parser fails right
      where this one stopped. *)

  val take_while1 : string -> (char -> bool) -> string t
  (** [take_while1 name pred]: non-empty longest prefix satisfying [pred];
      expects [name] (which names the whole token, e.g. ["integer"]) if there is
      none. Unlike [take_while ~label], adds nothing to later errors. *)

  val skip_while : ?label:string -> (char -> bool) -> unit t
  (** Like {!take_while}, discarding the text. *)

  (** {1 Characters}

      Predicates and parsers for ASCII character classes. Non-ASCII bytes belong
      to no class; see {!Utf8} for scalar-level parsing. None of these commit,
      and none leave a hint. *)

  val is_digit : char -> bool
  (** ['0'] to ['9']. *)

  val is_letter : char -> bool
  (** ['a'] to ['z'] and ['A'] to ['Z']. *)

  val is_alnum : char -> bool
  (** {!is_digit} or {!is_letter}. *)

  val is_hex_digit : char -> bool
  (** ['0'] to ['9'], ['a'] to ['f'] and ['A'] to ['F']. *)

  val is_space : char -> bool
  (** Space, tab, newline, carriage return and form feed (the set trimmed by
      [String.trim]). *)

  val digit : char t
  (** A character satisfying {!is_digit}; expects [digit]. *)

  val letter : char t
  (** A character satisfying {!is_letter}; expects [letter]. *)

  val hex_digit : char t
  (** A character satisfying {!is_hex_digit}; expects [hexadecimal digit]. *)

  val one_of : string -> char t
  (** [one_of "+-"] is one of the given characters; expects each of them as a
      literal, e.g. [expected '+' or '-'] . *)

  val none_of : string -> char t
  (** Any character except the given ones. Like {!any}, it expects nothing, so a
      failure reads [unexpected X]. *)

  val skip_line : unit t
  (** Skips through the next newline ([\n], so also [\r\n]), or to the end of
      the input. Never fails; the usual [~sync] for {!recover} in line-based
      formats. *)

  val newline : unit t
  (** ["\n"] or ["\r\n"]; expects [newline]. *)

  val whitespace : unit t
  (** Zero or more {!is_space} characters, {!hidden} from error messages. *)

  (** {1 Text} *)

  val string_ci : string -> string t
  (** Like {!string} but ASCII case-insensitive. Returns the input's text, not
      the argument. Expects the argument as a literal. *)

  val consumed : _ t -> string t
  (** [consumed p] runs [p] and returns the text it consumed. Failure,
      commitment and hints are those of [p]. *)

  (** {1 Numbers}

      Numbers are single tokens matched greedily: they either fail at their
      start, consuming nothing, or succeed without leaving a hint. A value too
      large for the type fails with a message ([integer out of range] or
      [number out of range]) that underlines the whole token. *)

  val natural : int t
  (** [[0-9]+]; expects [integer]. Prefer it to {!integer} in expression
      grammars that have a prefix minus operator. *)

  val integer : int t
  (** [-?[0-9]+]; expects [integer]. *)

  val number : float t
  (** [-?[0-9]+(\.[0-9]+)?([eE][+-]?[0-9]+)?]; expects [number]. An optional
      part is taken only if it is complete: on ["1.e5"] it matches ["1"]. A
      result that is infinite is out of range. *)

  (** {1 Strings} *)

  val quoted :
    ?escape:string t -> ?doubled:bool -> ?multiline:bool -> char -> string t
  (** [quoted ~escape q] is the text between two [q] characters on one line,
      with the quotes removed. Expects [string] at the opening quote. Raw bytes
      (including invalid UTF-8) are copied as they are.

      With [escape], a backslash starts an escape sequence: [escape] is run
      starting {e at the backslash} and its result replaces the sequence. If it
      fails without committing, complaining at the backslash or the byte after
      it, the failure becomes [invalid escape sequence], underlining the
      backslash and the character after it. Other failures of [escape] are
      reported unchanged. Without [escape], a backslash is an ordinary
      character.

      With [doubled] (default [false]), two quotes in a row inside the string
      stand for one [q], as in CSV: [quoted ~doubled:true] on a double quote
      reads two of them in a row as one double quote in the result. It combines
      with [escape].

      A newline ([\n] or [\r]) or the end of input before the closing quote,
      including right after a backslash, fails with [unterminated string],
      underlining the string from its opening quote to that point. With
      [multiline] (default [false]) raw newlines are part of the string instead,
      and only the end of input is unterminated; the error is reported at the
      end of input but underlines just the opening quote, since the rest of the
      text is probably not meant to be a string. Never commits, and leaves no
      hint. *)

  val json_escape : string t
  (** A JSON escape sequence starting at its backslash: a backslash followed by
      a double quote, backslash, slash, [b], [f], [n], [r] or [t]; or a [\uXXXX]
      escape. The result is UTF-8; a surrogate pair of [\u] escapes gives one
      scalar. An unknown escape expects [escape sequence] at the byte after the
      backslash. Malformed hexadecimal digits, or a surrogate that is not part
      of a pair, fail with [invalid unicode escape], underlining the offending
      escape (for a high surrogate followed by another [\u] escape, both). *)

  val string_literal : string t
  (** [quoted ~escape:json_escape '"']: a JSON-style string literal. *)

  (** {1 Whitespace, comments and tokens} *)

  (** Whitespace and comment policies. *)
  module Lexeme : sig
    (** The parsers of one policy. Each skips trailing [space] after the token
        it parses. *)
    module type S = sig
      val space : unit t
      (** Whitespace and comments, {!hidden} from error messages. A line comment
          stops before its newline. Fails with [unterminated comment], at the
          end of input and underlining the opening delimiter, if a block comment
          is not closed. *)

      val lexeme : 'a t -> 'a t
      (** [lexeme p] is [p] followed by {!space}. *)

      val symbol : string -> string t
      (** [symbol s] is {!string}[ s] followed by {!space}. *)

      val punct : string -> unit t
      (** [punct s] is [symbol s] returning [()], for punctuation and operators
          whose text is not needed: [and+ () = punct "="]. *)

      val keyword : string -> string t
      (** [keyword s] matches [s] only if it is not followed by an identifier
          character, then skips {!space}. Otherwise fails at its start,
          consuming nothing, expecting [s] as a literal: [keyword "if"] on
          ["iffy"] reports [expected "if", found "iffy"]. *)

      val kw : string -> unit t
      (** [kw s] is [keyword s] returning [()], for keywords whose text is not
          needed: [let+ () = kw "let" <* cut and+ name = identifier ...]. *)

      val identifier : string t
      (** An identifier start character followed by identifier characters,
          unless it is a reserved word, then {!space}. Otherwise fails at its
          start, consuming nothing, expecting [identifier]. *)
    end

    val make :
      ?line_comments:string list ->
      ?block_comments:(string * string) list ->
      ?nested_comments:bool ->
      ?newlines:bool ->
      ?ident_start:(char -> bool) ->
      ?ident_char:(char -> bool) ->
      ?reserved:string list ->
      unit ->
      (module S)
    (** [make ()] builds the parsers for a language, e.g.
        {[
        module L = (val Lexeme.make ~line_comments:[ "#" ] ())
        ]}

        - [line_comments]: delimiters of comments that run to the end of the
          line (default none).
        - [block_comments]: [(opening, closing)] delimiters (default none).
          Block openers are tried before line openers, so Lua's [--] and
          [--[[ ... ]]] work together.
        - [nested_comments]: whether block comments nest (default [false]).
        - [newlines]: whether newlines count as whitespace (default [true]).
          When [false], {!S.space} stops before ['\n'] and ['\r'], so a grammar
          can treat them as separators.
        - [ident_start], [ident_char]: identifier characters (defaults
          [[A-Za-z_]] and [[A-Za-z0-9_]]).
        - [reserved]: words that {!S.identifier} rejects (default none).

        Raises [Invalid_argument] if a delimiter is empty, or if comments nest
        and a block comment opens and closes with the same text. *)
  end

  (** {1 Unicode}

      Parsers over Unicode scalars of UTF-8 text. The other primitives of this
      module work on bytes and accept any input. Here an invalid byte is an
      error: the failure [invalid UTF-8] underlines it. The parsers that stop at
      a condition ({!Utf8.take_while} and friends) stop at an invalid byte
      instead of failing, and report [invalid UTF-8] if what follows fails at
      that point. *)
  module Utf8 : sig
    val any : Uchar.t t
    (** Any scalar. At the end of input it expects nothing. *)

    val char : Uchar.t -> Uchar.t t
    (** Exactly this scalar; expects it as a literal. *)

    val satisfy : string -> (Uchar.t -> bool) -> Uchar.t t
    (** [satisfy name pred]: a scalar satisfying [pred], expecting [name]. *)

    val take_while : ?label:string -> (Uchar.t -> bool) -> string t
    (** Like {!take_while} on scalars. *)

    val take_while1 : string -> (Uchar.t -> bool) -> string t
    (** Like {!take_while1} on scalars. *)

    val skip_while : ?label:string -> (Uchar.t -> bool) -> unit t
    (** Like {!skip_while} on scalars. *)
  end
end
