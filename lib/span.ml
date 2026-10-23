type t = { start : int; stop : int }

let make ~start ~stop =
  if start < 0 || stop < start then
    invalid_arg (Printf.sprintf "Span.make: invalid range %d-%d" start stop)
  else { start; stop }

let point i = make ~start:i ~stop:i
let length t = t.stop - t.start
let is_empty t = t.stop = t.start
let union a b = { start = min a.start b.start; stop = max a.stop b.stop }
let equal a b = a.start = b.start && a.stop = b.stop

let compare a b =
  match Int.compare a.start b.start with
  | 0 -> Int.compare a.stop b.stop
  | c -> c

let pp ppf t = Format.fprintf ppf "%d-%d" t.start t.stop
