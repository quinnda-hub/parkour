(** Structured problem reports.

    A diagnostic is data: where the problem is, what went wrong, what was
    expected, what was found, and which constructs were being parsed. Turning it
    into text with source excerpts is a separate step (rendering arrives with
    [Source] in the next milestone), so tools such as editors can consume the
    structured fields directly. *)

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

val make :
  ?severity:severity ->
  ?code:string ->
  ?expected:Expected.t list ->
  ?found:string ->
  ?context:(string * Span.t) list ->
  span:Span.t ->
  string ->
  t
(** [make ~span message] builds a diagnostic, e.g. for semantic checks run after
    parsing, so they render like syntax errors. [severity] defaults to [`Error];
    [expected] is sorted and deduplicated. *)

val is_error : t -> bool

val pp : Format.formatter -> t -> unit
(** Compact single-line form: [error[code] at 12-18: message]. *)
