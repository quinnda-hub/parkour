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

module type TOKEN = Parkour_intf.TOKEN

(** A parser over an array of tokens, as produced by {!tokenize}. Split parsing
    in two: a lexer turns text into tokens (with their spans), then a grammar
    over the tokens never sees whitespace or comments. Diagnostics still refer
    to the original text, so they render against the {!Source} the user wrote:
    underlines cover the source text of tokens, and [expected] lists use
    {!TOKEN.show}.

    {[
    type tok = Num of int | Plus

    module Tok = struct
      type t = tok

      let equal = ( = )
      let show = function Num n -> string_of_int n | Plus -> "+"
    end

    module T = Tokens (Tok)

    let lexer =
      let open Parkour in
      tokenize ~skip:(skip_while is_space)
        (choice
           [ map (fun n -> Num n) natural; map (fun _ -> Plus) (char '+') ])

    let sum =
      let open T in
      let num = expect "number" (function Num n -> Some n | _ -> None) in
      chainl1 num (token Plus *> return ( + ))

    let eval text =
      let source = Parkour.Source.make ~name:"calc" text in
      let report ds =
        List.iter
          (fun d -> print_string (Parkour.Diagnostic.render source d))
          ds
      in
      match lexer text with
      | Error ds -> report ds
      | Ok tokens -> (
          match T.parse sum (T.input ~source:text tokens) with
          | Ok n -> Printf.printf "%d\n" n
          | Error ds -> report ds)
    ]}

    A token's span is its source text; the end of the token array is the end of
    the source. A "found" description quotes the source text of the token, and
    {!S.not_followed_by} and friends describe a run of tokens by the source text
    from the first one's start to the last one's end. A token whose span is
    empty is quoted by its {!TOKEN.show} instead. *)
module Tokens (T : TOKEN) : sig
  include S with type token = T.t

  val input : source:string -> (T.t * Span.t) array -> input
  (** [input ~source tokens]: the tokens lexed from [source], with their spans
      in [source], in increasing order. *)
end

include TEXT
