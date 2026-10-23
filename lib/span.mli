(** Half-open ranges of byte offsets into the source text.

    Spans always refer to the original source, even when parsing a token stream,
    so diagnostics can be rendered against the text the user wrote. *)

type t = private { start : int; stop : int }
(** [start] is inclusive, [stop] exclusive, and [start <= stop]. An empty span
    marks a position between two bytes (e.g. end of input). *)

val make : start:int -> stop:int -> t
(** Raises [Invalid_argument] if [start < 0] or [stop < start]. *)

val point : int -> t
(** [point i] is the empty span at offset [i]. *)

val length : t -> int
val is_empty : t -> bool

val union : t -> t -> t
(** Smallest span covering both. *)

val equal : t -> t -> bool
val compare : t -> t -> int

val pp : Format.formatter -> t -> unit
(** Prints [start-stop] (byte offsets). Line/column rendering lives in
    {!Diagnostic}. *)
