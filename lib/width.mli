(* Internal: UTF-8 decoding with the "invalid byte = one character" rule, and
   approximate terminal display widths. Not exported from Parkour. *)

type decoded = private int
(** A decoded character packed into an int, to avoid allocation in scans. *)

val decode : string -> int -> decoded
(** [decode s i] for [0 <= i < String.length s]. A well-formed scalar gives its
    code point and byte length; any other byte gives code {!invalid} and length
    1. *)

val code : decoded -> int
val length : decoded -> int

val invalid : int
(** The [code] of an invalid byte (negative). *)

val of_code : int -> int
(** Display width (0, 1 or 2) of a code point, per the table in the rendering
    spec. *)
