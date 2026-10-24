(* A strict RFC 8259 JSON grammar written with Parkour, shared by json.ml (the
   command-line tool) and the JSONTestSuite corpus test.

   It shows the main techniques: mutually recursive parsers with
   [let rec ... = lazy] and [force], [trivia] whitespace, [cut] after an
   opening delimiter so errors inside it are not retried as something else,
   [between] so an unclosed bracket points back to where it was opened, and
   [context] for "while parsing ..." information.

   Strictness: whitespace is only space, tab, LF and CR (not form feed).
   Strings are [quoted] with [json_escape] for every JSON escape, including
   \u escapes and surrogate pairs, and reject raw control characters and
   invalid UTF-8. Numbers come from [number], which also reports out-of-range
   values, plus a check that rejects leading zeros. *)

open Parkour

type json =
  | Null
  | Bool of bool
  | Number of float
  | String of string
  | Array of json list
  | Object of (string * json) list

(* Every token eats the whitespace after it, so the grammar never mentions
   whitespace again; [trivia] hides it, so it never shows up in "expected ..."
   lists either. *)
let ws =
  trivia (skip_while (function ' ' | '\t' | '\n' | '\r' -> true | _ -> false))

let lexeme p = p <* ws
let sym c = lexeme (char c)

let str =
  lexeme (quoted ~escape:json_escape ~utf8:true ~forbid:(fun c -> c < ' ') '"')

(* [number] allows leading zeros, which JSON does not. [consumed] gives its
   text, and [validate] turns a bad one into a failure that underlines the
   whole token. *)
let no_leading_zero text =
  let digits =
    if text.[0] = '-' then String.sub text 1 (String.length text - 1) else text
  in
  if
    String.length digits > 1
    && digits.[0] = '0'
    && digits.[1] <= '9'
    && digits.[1] >= '0'
  then Error "leading zero in number"
  else Ok (float_of_string text)

let num = lexeme (validate no_leading_zero (consumed number))

let rec value =
  lazy
    (* [label] collapses the seven ways a value can start into one word. *)
    (choice ~label:"value"
       [
         lexeme (string "null") *> return Null;
         lexeme (string "true") *> return (Bool true);
         lexeme (string "false") *> return (Bool false);
         (num >>| fun n -> Number n);
         (str >>| fun s -> String s);
         force array;
         force obj;
       ])

(* [cut] right after the opening bracket: once we see [ or {, nothing else can
   be meant, so errors inside are reported as they are. [between] adds the
   "opened here" label when the closing bracket is missing. *)
and array =
  lazy
    (context "array"
       ( between (sym '[') (sym ']') (cut *> sep_by ~sep:(sym ',') (force value))
       >>| fun items -> Array items ))

and obj =
  lazy
    (let member =
       let+ key = str <* sym ':' and+ v = force value in
       (key, v)
     in
     context "object"
       ( between (sym '{') (sym '}') (cut *> sep_by ~sep:(sym ',') member)
       >>| fun members -> Object members ))

let json = ws *> force value
