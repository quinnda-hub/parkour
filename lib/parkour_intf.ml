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

  val parse : ?max_depth:int -> 'a t -> input -> ('a, Diagnostic.t list) result
  (** [parse p input] runs [p] and requires it to consume the whole input. On
      failure the list is non-empty and ordered by position. *)

  val parse_prefix :
    ?max_depth:int -> 'a t -> input -> ('a * int, Diagnostic.t list) result
  (** Like {!parse} but stops wherever [p] stops, also returning the input index
      reached. *)

  (** {1 Basic combinators} *)

  val return : 'a -> 'a t

  val fail : ?code:string -> string -> 'a t
  (** [fail msg] fails at the current position with a custom message. A custom
      message takes precedence over plain expectations at the same position.
      Does not commit. *)

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
end
