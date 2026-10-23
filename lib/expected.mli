(** Things a parser was prepared to accept at the point where it failed. *)

type t =
  | Literal of string
      (** Exact source text, e.g. [";"] or ["let"]. Rendered quoted: [';'],
          ["let"]. *)
  | Label of string
      (** A named category from {!S.label}, e.g. ["integer"]. Rendered bare. *)
  | End_of_input  (** Rendered as [end of input]. *)

val equal : t -> t -> bool

val compare : t -> t -> int
(** Total order used to sort and deduplicate expectation sets: labels, then
    literals, then end of input; alphabetical within a group. *)

val to_string : t -> string
(** Human form used in messages, with quoting and escaping applied. *)

val list_to_string : t list -> string
(** Joins the human forms as ["X"], ["X or Y"] or ["X, Y, or Z"]; empty for an
    empty list. *)
