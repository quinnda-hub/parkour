(* Internal: how source text is quoted and described in messages. Not exported
   from Parkour. *)

val literal : string -> string
(** Quote source text for "expected" lists. A single character (one byte, or one
    well-formed UTF-8 scalar) is single-quoted (except an apostrophe), anything
    else double-quoted:
    {v
    literal ";"   = {|';'|}
    literal "let" = {|"let"|}
    literal "é"   = {|'é'|}
    literal "'"   = {|"'"|}
    v}
    (A lone apostrophe is double-quoted, like a lone double quote is
    single-quoted, so neither needs escaping.) Escaped: newline, tab and
    carriage return as [\n], [\t], [\r]; backslash; the active quote character;
    other control bytes and invalid UTF-8 bytes as a lowercase [\xNN]. Printable
    ASCII and well-formed UTF-8 are kept verbatim. The empty string is quoted
    with double quotes. *)

val found : string -> int -> string * int
(** [found s i], for [0 <= i < String.length s], describes the input at byte [i]
    for "found ..." messages, returning the description and how many bytes it
    covers (always >= 1):
    - a run of word characters (ASCII alphanumerics, ['_'], and well-formed
      multi-byte UTF-8 scalars), capped at 32 bytes without splitting a scalar,
      is quoted with {!literal} (so a 1-character run is single-quoted),
      followed by […] if the run continues past the cap;
    - ['\n'] is [newline] (1 byte); ["\r\n"] is [newline] (2 bytes); a lone
      ['\r'] is [carriage return]; ['\t'] is [tab]; [' '] is [space];
    - anything else is one well-formed UTF-8 scalar (or one invalid byte),
      quoted with {!literal}. *)

val range : string -> int -> int -> string
(** [range s start stop], for [0 <= start < stop <= String.length s], describes
    the bytes from [start] up to (excluding) [stop] for "unexpected ..."
    messages: {!found}'s description if it covers exactly that range, otherwise
    {!literal} of the range truncated to 32 bytes (on a scalar boundary)
    followed by […] when truncated. Never looks past [stop]: the bytes of a
    scalar cut off by [stop] are escaped individually. *)
