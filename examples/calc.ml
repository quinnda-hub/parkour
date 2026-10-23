(* A calculator built with [expression].

   Run it on expressions given as arguments:  dune exec examples/calc.exe -- "1 + 2 * 3"
   Or on built-in samples:                    dune exec examples/calc.exe

   The table lists operator levels from the tightest to the loosest. A prefix
   operator may follow any operator ([2 ^ -1]), and its operand extends as far
   as its own level allows, so [-2 ^ 2] is [-(2 ^ 2)] because [^] is tighter.
   Comparisons are non-associative: [1 < 2 < 3] is an error. They yield 1 or 0. *)

open Parkour
module L = (val Lexeme.make ~line_comments:[ "#" ] ())

let op name f = L.symbol name >>| fun _ -> f
let truth b = if b then 1. else 0.

let expr =
  fix (fun expr ->
      let term =
        L.lexeme number <|> between (L.symbol "(") (L.symbol ")") expr
      in
      expression term
        [
          [ Infix_right (op "^" ( ** )) ];
          [ Prefix (op "-" Float.neg) ];
          [ Infix_left (op "*" ( *. )); Infix_left (op "/" ( /. )) ];
          [ Infix_left (op "+" ( +. )); Infix_left (op "-" ( -. )) ];
          [
            Infix_none (op "<" (fun a b -> truth (a < b)));
            Infix_none (op "=" (fun a b -> truth (a = b)));
          ];
        ])

let calculation = L.space *> expr

let eval text =
  match parse calculation text with
  | Ok v -> Printf.printf "%s  =>  %g\n" text v
  | Error diagnostics ->
      let source = Source.make ~name:"<input>" text in
      List.iter (fun d -> print_string (Diagnostic.render source d)) diagnostics

let samples =
  [
    "1 + 2 * 3";
    "2 ^ 3 ^ 2";
    "-2 ^ 2";
    "2 ^ -1";
    "(1 + 2) * 3 # the comment is skipped";
    "1 + 1 = 2";
    "1 < 2 < 3";
    "1 + * 2";
    "(1 + 2";
  ]

let () =
  match List.tl (Array.to_list Sys.argv) with
  | [] -> List.iter eval samples
  | args -> List.iter eval args
