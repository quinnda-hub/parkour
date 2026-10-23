(** Source text with a line index, for turning byte offsets into positions
    people (and editors) understand.

    Creating a source is cheap: the line index is built the first time a
    position is asked for, so it costs nothing on the success path.

    Lines end at ["\n"] or ["\r\n"]; a lone ["\r"] is an ordinary character.
    Columns count Unicode characters (scalar values), not bytes; each byte of
    invalid UTF-8 counts as one character. *)

type t

val make : ?name:string -> string -> t
(** [make ~name text]. [name] (typically a file name) is used in rendered
    locations. *)

val name : t -> string option
val text : t -> string

type position = {
  offset : int;  (** byte offset, 0-based *)
  line : int;  (** 1-based *)
  column : int;  (** 1-based, in characters *)
}

val position : t -> int -> position
(** [position src offset], for [0 <= offset <= String.length (text src)]. An
    offset inside a multi-byte character gets that character's column. The
    column stops at the end of the line's content, so the ["\n"] of a ["\r\n"]
    gets the same position as its ["\r"]. Raises [Invalid_argument] for offsets
    out of range. *)

val offset : t -> line:int -> column:int -> int
(** Inverse of {!val-position} for offsets at character boundaries, except the
    ["\n"] of a ["\r\n"] (which maps to the position of its ["\r"]). Columns
    past the end of the line clamp to the line's end. Raises [Invalid_argument]
    if [line] is out of range or [column < 1]. *)

val char_offset : t -> int -> int
(** Number of characters before the byte offset, counting every character
    (including carriage returns). An offset inside a multi-byte character does
    not count that character. Raises [Invalid_argument] if the offset is out of
    range. *)

val utf16_column : t -> int -> int
(** 0-based column of the byte offset in UTF-16 code units, as used by the
    Language Server Protocol. Like the column of {!val-position}, it stops at
    the end of the line's content. Each invalid byte counts as one unit (editors
    may differ). Raises [Invalid_argument] if the offset is out of range. *)

val line_count : t -> int
(** Number of lines; a text ending in a newline has an empty last line. *)

val line_span : t -> int -> Span.t
(** [line_span src line]: the bytes of the 1-based [line], without its
    terminator. *)
