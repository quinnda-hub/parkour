type severity = [ `Error | `Warning ]

type t = {
  severity : severity;
  code : string option;
  span : Span.t;
  message : string;
  expected : Expected.t list;
  found : string option;
  context : (string * Span.t) list;
}

let severity t = t.severity
let code t = t.code
let span t = t.span
let message t = t.message
let expected t = t.expected
let found t = t.found
let context t = t.context

let make ?(severity = `Error) ?code ?(expected = []) ?found ?(context = [])
    ~span message =
  let expected = List.sort_uniq Expected.compare expected in
  { severity; code; span; message; expected; found; context }

let is_error t = t.severity = `Error

let pp ppf t =
  let sev = match t.severity with `Error -> "error" | `Warning -> "warning" in
  let code = match t.code with None -> "" | Some c -> "[" ^ c ^ "]" in
  Format.fprintf ppf "%s%s at %a: %s" sev code Span.pp t.span t.message
