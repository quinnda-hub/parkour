(** Shared QCheck2 helpers for the Parkour test suites.

    All tuning lives here. The fast tier uses [default_count] cases of the base
    sizes; [--long] multiplies the counts by [default_long_factor] and scales
    the generator sizes that go through {!scale}, {!bound} and {!range}.
    Fixed-size stress checks use {!once}: they run once in both tiers. *)

val default_count : int
(** Cases per property in the fast tier. *)

val default_long_factor : int
(** Multiplier applied to counts when running with [--long]. *)

val long : unit -> bool
(** Whether the run is the [--long] one (known once the runner has parsed its
    command line, so call it when generating, not when defining). *)

exception Runaway
(** Raised by {!tick} when a case has used up its fuel. *)

val tick : unit -> unit
(** Called where a generated parser runs a loop body: each case has a budget of
    a hundred thousand ticks, after which the case fails with {!Runaway}. This
    keeps a regression in zero-width detection a failure, not a hang. *)

val scale : ?num:int -> ?den:int -> int -> int
(** [scale n] is [n] in the fast tier, and [n * num / den] (default [3n], at
    least [n + 1]) under [--long]. *)

val bound : ?num:int -> ?den:int -> int -> int QCheck2.Gen.t
(** [bound n] is [int_bound (scale n)], scaled when generating. *)

val range : ?num:int -> ?den:int -> int -> int -> int QCheck2.Gen.t
(** [range lo hi] is [int_range lo (scale hi)], scaled when generating. *)

val test :
  ?count:int ->
  ?print:('a -> string) ->
  ?collect:('a -> string) ->
  name:string ->
  'a QCheck2.Gen.t ->
  ('a -> bool) ->
  QCheck2.Test.t
(** [test ~name gen prop] builds a property test using the shared defaults.
    [collect] tags each case, to measure how often the useful cases occur: it
    only takes effect when the environment variable [PARKOUR_COVERAGE] is set,
    and the histogram is then printed with the results. *)

val once : name:string -> (unit -> bool) -> QCheck2.Test.t
(** A check of fixed size: run once, in the fast and the long tier alike. *)

val run : string -> QCheck2.Test.t list -> unit
(** [run suite_name tests] runs the tests (honouring QCheck's command line,
    including [--long]) and exits non-zero on failure. *)
