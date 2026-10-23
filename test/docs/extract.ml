(* Extracts the blocks of an odoc page that are tested: [extract code page]
   prints its [{[ ... ]}] blocks as one OCaml program, with line directives
   pointing back into the page; [extract output page] prints its [{v ... v}]
   blocks, the program's expected output. Blocks are delimited by lines that
   are exactly the opening and closing markers. *)

let blocks ~opening ~closing lines =
  let rec go acc current = function
    | [] -> List.rev acc
    | (_, line) :: rest when line = closing && current <> None ->
        let start, body = Option.get current in
        go ((start, List.rev body) :: acc) None rest
    | (n, line) :: rest when line = opening && current = None ->
        go acc (Some (n + 1, [])) rest
    | (_, line) :: rest -> (
        match current with
        | Some (start, body) -> go acc (Some (start, line :: body)) rest
        | None -> go acc None rest)
  in
  go [] None lines

(* The page as named from the project root, for compiler messages: dune
   passes it relative to the build directory, as [../../doc/page.mld]. *)
let rec source_name path =
  if String.length path > 3 && String.sub path 0 3 = "../" then
    source_name (String.sub path 3 (String.length path - 3))
  else path

let () =
  match Sys.argv with
  | [| _; mode; page |] ->
      let ic = open_in_bin page in
      let rec read n acc =
        match input_line ic with
        | line -> read (n + 1) ((n, line) :: acc)
        | exception End_of_file -> List.rev acc
      in
      let lines = read 1 [] in
      close_in ic;
      let print_lines = List.iter print_endline in
      if mode = "code" then
        List.iter
          (fun (start, body) ->
            Printf.printf "# %d %S\n" start (source_name page);
            print_lines body)
          (blocks ~opening:"{[" ~closing:"]}" lines)
      else
        List.iter
          (fun (_, body) -> print_lines body)
          (blocks ~opening:"{v" ~closing:"v}" lines)
  | _ ->
      prerr_endline "usage: extract (code | output) PAGE";
      exit 2
