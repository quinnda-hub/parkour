let default_count = 100
let default_long_factor = 20
let long () = QCheck_base_runner.long_tests ()

(* Fuel for a case: parsers built by the generators call [tick] where a loop
   body runs, so that a library that stops rejecting zero-width repetition
   makes the case fail instead of hanging the suite. *)
exception Runaway

let fuel = 100_000
let ticks = ref 0

let tick () =
  incr ticks;
  if !ticks > fuel then raise Runaway

let fresh prop x =
  ticks := 0;
  prop x

let scale ?(num = 3) ?(den = 1) n =
  if long () then max (n + 1) (n * num / den) else n

let bound ?num ?den n =
  QCheck2.Gen.(
    let* () = unit in
    int_bound (scale ?num ?den n))

let range ?num ?den lo hi =
  QCheck2.Gen.(
    let* () = unit in
    int_range lo (scale ?num ?den hi))

(* The histogram of [collect] is printed on every run, so it is kept for
   measuring: set PARKOUR_COVERAGE. *)
let measuring = Sys.getenv_opt "PARKOUR_COVERAGE" <> None

let test ?(count = default_count) ?print ?collect ~name gen prop =
  let collect = if measuring then collect else None in
  QCheck2.Test.make ~count ~long_factor:default_long_factor ~name ?print
    ?collect gen (fresh prop)

let once ~name f =
  QCheck2.Test.make ~count:1 ~long_factor:1 ~name QCheck2.Gen.unit (fresh f)

let run name tests =
  print_endline ("suite: " ^ name);
  QCheck_base_runner.run_tests_main tests
