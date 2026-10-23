(** Structured problem reports.

    A diagnostic is data: where the problem is, what went wrong, what was
    expected, what was found, and which constructs were being parsed. Tools such
    as editors can consume these fields directly; {!render} turns a diagnostic
    into a human-readable report with a source excerpt:
    {v
    error: expected integer, found "banana"
     --> config.txt:4:8
      |
    4 | ammo = banana
      |        ^^^^^^ expected integer
      = while parsing assignment, from 4:1
    v} *)

type severity = [ `Error | `Warning ]
type t

val severity : t -> severity

val code : t -> string option
(** Stable machine-readable identifier given via [fail ~code], if any. *)

val span : t -> Span.t
(** Primary location: what to underline. *)

val message : t -> string
(** One-line headline, e.g. [expected integer, found "banana"] or the message
    passed to [fail]. Lowercase, no trailing period. *)

val expected : t -> Expected.t list
(** Sorted, without duplicates. Empty for custom failures. *)

val found : t -> string option
(** Description of the unexpected input (already quoted), e.g. ["\"banana\""],
    ["newline"] or ["end of input"]. [None] for custom failures. *)

val context : t -> (string * Span.t) list
(** Constructs being parsed when the problem occurred, innermost first, each
    with the span from where it started to where the problem occurred. See
    {!S.context}. *)

val label : t -> string option
(** Text shown beside the primary underline, if given explicitly. When absent,
    {!render} shows [expected ...] if there are expectations. *)

val labels : t -> (Span.t * string) list
(** Secondary labels: other places that help explain the problem (say, where an
    unclosed bracket was opened), in the order given. *)

val notes : t -> string list
(** Free-form hints shown after the source excerpt, e.g. how to fix the problem.
*)

val make :
  ?severity:severity ->
  ?code:string ->
  ?expected:Expected.t list ->
  ?found:string ->
  ?context:(string * Span.t) list ->
  ?label:string ->
  ?labels:(Span.t * string) list ->
  ?notes:string list ->
  span:Span.t ->
  string ->
  t
(** [make ~span message] builds a diagnostic, e.g. for semantic checks run after
    parsing, so they render like syntax errors. [severity] defaults to [`Error];
    [expected] is sorted and deduplicated. [label] is the text for the primary
    underline, [labels] are secondary spans with their texts (which may be
    empty), and [notes] are hints. See {!render}. *)

val add_label : Span.t -> string -> t -> t
(** [add_label span text d] appends a secondary label. *)

val add_note : string -> t -> t
(** [add_note text d] appends a note. *)

val is_error : t -> bool

(** {1 Printing} *)

type location = [ `Line_column | `Char_offset | `Byte_offset ]
(** How positions are shown: [4:8] (line and column, the default), [char 52]
    (characters from the start) or [byte 57]. *)

val pp : Format.formatter -> t -> unit
(** Compact single-line form with byte positions:
    [bytes 57-63: error[E42]: message] ([byte 57] for an empty span). *)

val pp_located : ?location:location -> Source.t -> Format.formatter -> t -> unit
(** Compact single-line form with real positions, as used by compilers and
    understood by editors: [config.txt:4:8: error[E42]: message]. Use it as
    [Format.printf "%a" (Diagnostic.pp_located source) d]. *)

val render :
  ?location:location ->
  ?color:bool ->
  ?max_context:int ->
  ?tab_width:int ->
  ?max_width:int ->
  Source.t ->
  t ->
  string
(** Multi-line report with a source excerpt, in the style shown above.
    - [color] (default [false]) adds ANSI colours.
    - [max_context] (default [2]): how many of the innermost {!context} frames
      to show; the outermost frame is shown too, and the number of omitted
      frames is noted.
    - [tab_width] (default [4]) for aligning underlines after tabs.
    - [max_width] (default [100]): source lines wider than this many columns are
      shown as a window around the problem, with […] marking the cuts.

    Underlines are aligned for wide (e.g. CJK) and zero-width (combining)
    characters using a built-in approximation of terminal widths. Underlines
    stop before trailing whitespace in the span (such as what a lexeme skipped),
    unless the span is only whitespace. The result ends with a newline.

    Secondary {!labels} are underlined with [-] on their own rows, below the
    source line they touch (the lines of all spans are shown, with [...] where
    lines are skipped):
    {v
    error: expected ',' or ']', found '5'
     --> 3:4
      |
    1 | [1, 2,
      | - opened here
    ...
    3 |  4 5]
      |    ^ expected ',' or ']'
    v}
    {!notes} follow the context frames. Rows of one line are ordered by the
    start column of their underline. *)

val pp_rendered :
  ?location:location ->
  ?color:bool ->
  ?max_context:int ->
  ?tab_width:int ->
  ?max_width:int ->
  Source.t ->
  Format.formatter ->
  t ->
  unit
(** {!render} for [Format]. It prints a multi-line block ending in a newline, so
    it is meant for top-level use, not inside [Format] boxes. *)
