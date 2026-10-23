(* The Stack_overflow backstop. Run with a small stack limit (see the dune
   rule), which the runtime enforces on OCaml 5 and in 4.14 bytecode. In 4.14
   native code the stack is the system's: whether overflowing it raises
   [Stack_overflow], and how deep it goes, depends on the platform and its
   ulimit, so the test is skipped there. Kept apart from the other suites so
   that the limit does not affect them. *)

module P = Parkour
module D = P.Diagnostic
module Prop = Parkour_test_support.Prop

(* 20 [context] wrappers per level: the stack used by a level is large
   compared to what [max_depth] accounts for. *)
let deep =
  let open P in
  fix (fun self ->
      let inner = between (char '(') (char ')') self <|> natural in
      List.fold_left
        (fun p _ -> context "w" (p <* return ()))
        inner (List.init 20 Fun.id))

let exhausted d =
  match
    Scanf.sscanf (D.message d)
      "nesting too deep (stack exhausted after %d levels)%!" Fun.id
  with
  | n -> Some n
  | exception _ -> None

let backstop =
  Prop.once
    ~name:"a stack overflow is a nesting diagnostic at the deepest entry"
    (fun () ->
      let input = String.make 1_000_000 '(' in
      let deepest_ok = function
        | Some [ d ] -> (
            match exhausted d with
            | Some n -> n > 0 && (D.span d).start = n - 1
            | None -> false)
        | _ -> false
      in
      let result f = match f () with Ok _ -> None | Error ds -> Some ds in
      deepest_ok (result (fun () -> P.parse ~max_depth:max_int deep input))
      && (match P.parse_partial ~max_depth:max_int deep input with
        | None, [ d ] -> exhausted d <> None
        | _ -> false)
      && (match P.parse_prefix ~max_depth:max_int deep input with
        | Error [ d ] -> exhausted d <> None
        | _ -> false)
      (* With the default limit it is a nesting diagnostic whichever of the
         limit and the stack runs out first. *)
      && (match P.parse deep input with
        | Error [ d ] ->
            String.starts_with ~prefix:"nesting too deep" (D.message d)
        | _ -> false)
      (* The state of one run does not leak into the next. *)
      && P.parse deep "((1))" = Ok 1
      && P.parse deep "1" = Ok 1)

let () =
  if Sys.ocaml_release.major < 5 && Sys.backend_type = Sys.Native then
    print_endline
      "stack: skipped (OCaml 4.14 native code uses the system stack)"
  else Prop.run "stack" [ backstop ]
