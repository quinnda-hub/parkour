let default_count = 100
let default_long_factor = 20

let test ?(count = default_count) ?print ~name gen prop =
  QCheck2.Test.make ~count ~long_factor:default_long_factor ~name ?print gen
    prop

let run name tests =
  print_endline ("suite: " ^ name);
  QCheck_base_runner.run_tests_main tests
