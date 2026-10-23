(** Random grammars over the alphabet [{a,b,c}], compiled to Parkour parsers and
    interpreted by an independent naive reference oracle. Every grammar yields a
    string value. *)

type seq_style =
  | By_bind  (** compiled with [bind] *)
  | By_both  (** compiled with [both] *)
  | By_ap  (** compiled with [<*>] *)

type t =
  | Char of char
  | Str of string
  | Satisfy of string * int  (** label, bitmask over a/b/c *)
  | Any
  | Eof
  | Seq of seq_style * t * t  (** concatenates the values *)
  | Choice of t list  (** like folding [Alt]; [Choice []] always fails *)
  | Alt of t * t
  | Many of t
  | Many1 of t
  | Opt of t
  | Sep_by of t * t  (** separator, item; the value is the items' text only *)
  | Sep_end_by of t * t  (** separator, item; value is the items' text only *)
  | Many_till of t * t  (** item, stop; value is the items' text only *)
  | Count of int * t
  | Label of string * t
  | Hidden of t
  | Fail of string
  | Cut
  | Attempt of t
  | Lookahead of t
  | Not_followed_by of t
  | Take_while of string option * int
  | Take_while1 of string * int

val to_string : t -> string

val gen : cuts:bool -> t QCheck2.Gen.t
(** Shrinkable, size-bounded. [Cut] only appears when [cuts]. Nullable
    repetitions (hence [Grammar_error]) occur naturally. *)

val gen_input : string QCheck2.Gen.t
(** Strings over [{a,b,c}] of length at most 8. *)

val compile : t -> string Parkour.t

(** The same compilation over any parser on [char] tokens, for comparing inputs.
    Text-only primitives ([Str], [Take_while], [Take_while1]) are rewritten into
    token-level combinations, so every instance compiles the same grammar (not
    the same as {!compile}). *)
module Compile (P : Parkour.S with type token = char) : sig
  val compile : t -> string P.t
end

val children : t -> t list
(** Immediate sub-grammars. *)

val erase_cuts : t -> t
(** Replaces every [Cut] by an equally nullable parser that does not commit. *)

(** The oracle. Both functions raise [Parkour.Grammar_error] exactly when a
    zero-width repetition iteration is reached. *)
module Oracle : sig
  type failure = {
    pos : int;
    expected : Parkour.Expected.t list;
        (** sorted, deduplicated; [[]] for custom *)
    custom : string option;
    unexpected : int option;
        (** end of the matched range, for [not_followed_by] *)
  }

  type outcome = Ok of string * int | Error of failure

  val run_prefix : t -> string -> outcome
  (** Like [parse_prefix]. *)

  val run : t -> string -> outcome
  (** Like [parse]: requires end of input; the position is the final one. *)
end
