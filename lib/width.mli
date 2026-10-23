(* Internal: approximate terminal display widths. Not exported from Parkour. *)

val of_code : int -> int
(** Display width (0, 1 or 2) of a code point, per the table in the rendering
    spec. *)
