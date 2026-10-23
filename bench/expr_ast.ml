(* The expression tree every implementation builds: [Bin] carries the
   operator character. *)
type t = Int of int | Neg of t | Bin of char * t * t
