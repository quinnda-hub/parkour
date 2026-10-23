(** Parser combinators for human-authored text, with precise, structured syntax
    errors.

    {[
    open Parkour

    let is_ident = function 'a' .. 'z' | '_' -> true | _ -> false
    let is_digit = function '0' .. '9' -> true | _ -> false
    let is_space c = c = ' '

    let assignment =
      context "assignment"
        (let+ name = take_while1 "identifier" is_ident
         and+ () = skip_while is_space *> char '=' *> cut *> skip_while is_space
         and+ value = take_while1 "integer" is_digit in
         (name, int_of_string value))

    let () =
      let input = "ammo = banana" in
      match parse assignment input with
      | Ok (name, n) -> Printf.printf "%s = %d\n" name n
      | Error diagnostics ->
          let source = Source.make ~name:"config" input in
          List.iter
            (fun d -> print_string (Diagnostic.render source d))
            diagnostics
    ]}

    The top level of this module is the parser over [string] input; see
    {!module-type-S} for the combinators and their semantics, and {!Make} to
    parse other input such as token streams. *)

module Span = Span
module Expected = Expected

module Source = Source
(** Source text with a line index, for positions and rendering. *)

module Diagnostic = Diagnostic

exception Grammar_error of string
(** Raised (never returned) when a grammar is broken regardless of input, e.g.
    [many] applied to a parser that succeeds without consuming input, which
    would otherwise repeat forever. *)

module type INPUT = Parkour_intf.INPUT
module type S = Parkour_intf.S
module type TEXT = Parkour_intf.TEXT

(** A parser over any input. *)
module Make (I : INPUT) : S with type input = I.t and type token = I.token

include TEXT

(**/**)

(** Internals exposed for Parkour's own tests. Not part of the API. *)
module Private : sig
  module Quote = Quote
end
