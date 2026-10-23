(* Benchmarks: Parkour against other OCaml parsers.

   dune exec --profile release bench/main.exe -- [--quick] [--errors]

   Without flags: a throughput/allocation table (MB/s and minor words per input
   byte) for JSON and for arithmetic expressions. [--quick] shortens the time
   per measurement. [--errors] prints each library's message on broken JSON. *)

open Bechamel

let read_file path =
  let ic = open_in_bin path in
  let s = really_input_string ic (in_channel_length ic) in
  close_in ic;
  s

(* ---- Measurement ---- *)

type row = { impl : string; mb_s : float; minor : float; major : float }

let instances =
  Toolkit.Instance.[ monotonic_clock; minor_allocated; major_allocated ]

(* Per-run estimate of one measure, by ordinary least squares over runs. *)
let estimate quota f =
  let test = Test.make ~name:"t" (Staged.stage f) in
  let cfg = Benchmark.cfg ~limit:2000 ~quota:(Time.second quota) ~kde:None () in
  let raw = Benchmark.all cfg instances test in
  let ols =
    Analyze.ols ~bootstrap:0 ~r_square:false ~predictors:[| Measure.run |]
  in
  List.map
    (fun inst ->
      let r = Hashtbl.find (Analyze.all ols inst raw) "t" in
      match Analyze.OLS.estimates r with Some [ x ] -> x | _ -> nan)
    instances

let measure quota ~bytes impl f =
  match estimate quota f with
  | [ ns; minor; major ] ->
      let b = float_of_int bytes in
      { impl; mb_s = b /. ns *. 1000.; minor = minor /. b; major = major /. b }
  | _ -> assert false

let print_table title rows =
  Printf.printf "\n%s\n" title;
  Printf.printf "  %-18s %10s %18s %18s\n" "implementation" "MB/s"
    "minor words/byte" "major words/byte";
  List.iter
    (fun r ->
      Printf.printf "  %-18s %10.1f %18.3f %18.3f\n" r.impl r.mb_s r.minor
        r.major)
    rows;
  flush stdout

(* Every implementation must build the same tree as the first one (Parkour). *)
let check name src impls =
  let results = List.map (fun (n, f) -> (n, f src)) impls in
  match results with
  | [] -> ()
  | (_, reference) :: rest ->
      (match reference with
      | Ok _ -> ()
      | Error e -> failwith (Printf.sprintf "%s: parkour failed: %s" name e));
      List.iter
        (fun (n, r) ->
          if r <> reference then
            failwith
              (Printf.sprintf "%s: %s disagrees with parkour (%s)" name n
                 (match r with Ok _ -> "different tree" | Error e -> e)))
        rest

let bench_inputs quota title inputs impls =
  List.iter
    (fun (name, src) ->
      check name src impls;
      let bytes = String.length src in
      let rows =
        List.map
          (fun (impl, f) ->
            measure quota ~bytes impl (fun () -> ignore (f src)))
          impls
      in
      print_table (Printf.sprintf "%s: %s (%d bytes)" title name bytes) rows)
    inputs

(* ---- Error comparison ---- *)

(* The broken inputs of examples/json.ml's gallery. *)
let broken =
  [
    ("trailing comma in an array", {|[1, 2, 3,]|});
    ("trailing comma in an object", {|{"a": 1, "b": 2,}|});
    ("single-quoted string", {|{"name": 'parkour'}|});
    ("unquoted key", {|{name: "parkour"}|});
    ("NaN", {|[1, NaN]|});
    ("a // comment", "{\n  \"a\": 1 // the answer\n}");
    ("missing comma between members", "{\n  \"a\": 1\n  \"b\": 2\n}");
    ("missing colon", {|{"a" 1}|});
    ("unclosed [ spanning several lines", "[\n  1,\n  2,\n  3\n");
    ("unclosed { before more input", "{\n  \"a\": [1, 2]\n  \"b\": 3\n");
    ("unterminated string", {|{"name": "parkour}|});
    ("invalid escape", {|["tab\q"]|});
    ("empty input", "");
    ("trailing garbage after the value", {|{"a": 1} {"b": 2}|});
  ]

(* One line per message: newlines and runs of blanks become single spaces. *)
let one_line s =
  String.split_on_char '\n' s
  |> List.concat_map (String.split_on_char ' ')
  |> List.filter (fun w -> w <> "")
  |> String.concat " "

let run_errors () =
  List.iter
    (fun (title, src) ->
      Printf.printf "=== %s ===\n%s\n" title
        (String.concat "\n"
           (List.map (fun l -> "  | " ^ l) (String.split_on_char '\n' src)));
      List.iter
        (fun (name, f) ->
          match f src with
          | Ok _ -> Printf.printf "  %-16s (accepted)\n" name
          | Error e -> Printf.printf "  %-16s %s\n" name (one_line e))
        Json_impls.all;
      print_newline ())
    broken

(* ---- Main ---- *)

let run_bench quota =
  let fixtures =
    List.map
      (fun n ->
        let path = Printf.sprintf "bench/data/%s.json" n in
        if not (Sys.file_exists path) then (
          prerr_endline
            "bench/data/ is missing: run bench/fetch.sh from the repository \
             root first.";
          exit 1);
        (n ^ ".json", read_file path))
      [ "canada"; "twitter"; "citm_catalog" ]
  in
  print_endline
    "JSON. yojson+convert includes the conversion of Yojson.Basic.t to the \
     shared tree.";
  bench_inputs quota "json" fixtures Json_impls.all;
  print_endline "\nExpressions (generated, fixed seed).";
  bench_inputs quota "expr"
    [ ("generated", Expr_impls.generate ()) ]
    Expr_impls.all

let () =
  let args = List.tl (Array.to_list Sys.argv) in
  if List.mem "--errors" args then run_errors ()
  else run_bench (if List.mem "--quick" args then 0.3 else 2.0)
