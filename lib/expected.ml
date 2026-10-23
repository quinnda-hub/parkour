type t = Literal of string | Label of string | End_of_input

let rank = function Label _ -> 0 | Literal _ -> 1 | End_of_input -> 2

let compare a b =
  match (a, b) with
  | Label x, Label y | Literal x, Literal y -> String.compare x y
  | _ -> Int.compare (rank a) (rank b)

let equal a b = compare a b = 0

let to_string = function
  | Literal s -> Quote.literal s
  | Label s -> s
  | End_of_input -> "end of input"
