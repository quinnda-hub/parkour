(** Shared QCheck2 helpers for the Parkour test suites. *)

val default_count : int
(** Cases per property in the fast tier. *)

val default_long_factor : int
(** Multiplier applied to counts when running with [--long]. *)

val test :
  ?count:int ->
  ?print:('a -> string) ->
  name:string ->
  'a QCheck2.Gen.t ->
  ('a -> bool) ->
  QCheck2.Test.t
(** [test ~name gen prop] builds a property test using the shared defaults. *)

val run : string -> QCheck2.Test.t list -> unit
(** [run suite_name tests] runs the tests (honouring QCheck's command line,
    including [--long]) and exits non-zero on failure. *)
