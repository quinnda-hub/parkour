(* Runs the strict JSON grammar over the JSONTestSuite corpus: y_* files must
   parse, n_* files must be rejected, i_* files may go either way. No file may
   raise. *)

let corpus = "corpus"

(* Files where the grammar deliberately disagrees with the suite: file name and
   reason. *)
let allowlist : (string * string) list = []

let read file =
  let ic = open_in_bin (Filename.concat corpus file) in
  let src = really_input_string ic (in_channel_length ic) in
  close_in ic;
  src

let () =
  let files =
    Sys.readdir corpus |> Array.to_list
    |> List.filter (fun f -> Filename.check_suffix f ".json")
    |> List.sort compare
  in
  let count c = List.length (List.filter (fun f -> f.[0] = c) files) in
  let failures =
    ref (if (count 'y', count 'n', count 'i') = (95, 188, 35) then 0 else 1)
  in
  if !failures > 0 then
    Printf.printf
      "unexpected corpus: %d y_, %d n_, %d i_ (expected 95/188/35)\n"
      (count 'y') (count 'n') (count 'i');
  let accepted_i = ref 0 and rejected_i = ref 0 in
  let fail file msg =
    incr failures;
    Printf.printf "%s: %s\n" file msg
  in
  List.iter
    (fun file ->
      let result =
        match Parkour.parse Json_parser.json (read file) with
        | r -> Ok r
        | exception e -> Error (Printexc.to_string e)
      in
      let expected = file.[0] in
      match result with
      | Error e -> fail file ("exception: " ^ e)
      | Ok _ when List.mem_assoc file allowlist -> ()
      | Ok (Ok _) -> (
          match expected with
          | 'n' -> fail file "accepted, expected rejection"
          | 'i' -> incr accepted_i
          | _ -> ())
      | Ok (Error ds) -> (
          match expected with
          | 'y' ->
              let msg =
                match ds with
                | d :: _ -> Parkour.Diagnostic.message d
                | [] -> "(no diagnostic)"
              in
              fail file ("rejected, expected acceptance: " ^ msg)
          | 'i' -> incr rejected_i
          | _ -> ()))
    files;
  Printf.printf "%d files, %d failures (i_: %d accepted, %d rejected)\n"
    (List.length files) !failures !accepted_i !rejected_i;
  if !failures > 0 then exit 1
