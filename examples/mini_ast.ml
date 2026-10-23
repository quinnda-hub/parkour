(* Shared by the two versions of the Mini example (mini.ml parses the text
   directly, mini_tokens.ml lexes first): the syntax tree, a printer for it,
   and the broken programs of the error gallery. No parsing happens here.

   Mini is a small C-like language:

     // line comments
     fn fib(n) {
       if n < 2 { return n; } else { return fib(n - 1) + fib(n - 2); }
     }
     let i = 0;
     while i < 10 { print(fib(i)); i = i + 1; }
*)

type expr =
  | Int of int
  | Str of string
  | Bool of bool
  | Var of string
  | Call of string * expr list
  | Unary of string * expr
  | Binary of string * expr * expr

type stmt =
  | Let of string * expr
  | Assign of string * expr
  | If of expr * stmt list * stmt list
    (* [else if] is an [If] alone in the else branch *)
  | While of expr * stmt list
  | Return of expr option
  | Expr of expr
  | Recovered  (** A statement that was skipped after an error. *)

type func = { name : string; params : string list; body : stmt list }
type item = Fn of func | Stmt of stmt
type program = item list

(* ---- Printing: the program as normalised source ---- *)

(* Binary operations are fully parenthesised, which shows how the operator
   table grouped them. *)
let rec show_expr = function
  | Int n -> string_of_int n
  | Str s -> Printf.sprintf "%S" s
  | Bool b -> string_of_bool b
  | Var x -> x
  | Call (f, args) ->
      f ^ "(" ^ String.concat ", " (List.map show_expr args) ^ ")"
  | Unary (op, e) -> op ^ show_expr e
  | Binary (op, a, b) -> "(" ^ show_expr a ^ " " ^ op ^ " " ^ show_expr b ^ ")"

let rec print_stmt indent s =
  let line fmt = Printf.ksprintf (fun l -> print_endline (indent ^ l)) fmt in
  let block body = List.iter (print_stmt (indent ^ "  ")) body in
  match s with
  | Let (x, e) -> line "let %s = %s;" x (show_expr e)
  | Assign (x, e) -> line "%s = %s;" x (show_expr e)
  | Return None -> line "return;"
  | Return (Some e) -> line "return %s;" (show_expr e)
  | Expr e -> line "%s;" (show_expr e)
  | Recovered -> line "<error>;"
  | While (c, body) ->
      line "while %s {" (show_expr c);
      block body;
      line "}"
  | If (c, yes, no) ->
      line "if %s {" (show_expr c);
      print_if_tail indent yes no

(* The rest of an [if] after its condition, printing [else if] chains flat. *)
and print_if_tail indent yes no =
  let line fmt = Printf.ksprintf (fun l -> print_endline (indent ^ l)) fmt in
  let block body = List.iter (print_stmt (indent ^ "  ")) body in
  block yes;
  match no with
  | [] -> line "}"
  | [ If (c, yes, no) ] ->
      line "} else if %s {" (show_expr c);
      print_if_tail indent yes no
  | _ ->
      line "} else {";
      block no;
      line "}"

let print_item = function
  | Stmt s -> print_stmt "" s
  | Fn f ->
      Printf.printf "fn %s(%s) {\n" f.name (String.concat ", " f.params);
      List.iter (print_stmt "  ") f.body;
      print_endline "}"

let print_program = List.iter print_item

(* ---- Summary: "2 functions, 5 statements (1 recovered)" ---- *)

let rec count_stmt (total, bad) s =
  let block acc = List.fold_left count_stmt acc in
  match s with
  | Recovered -> (total + 1, bad + 1)
  | If (_, yes, no) -> block (block (total + 1, bad) yes) no
  | While (_, body) -> block (total + 1, bad) body
  | _ -> (total + 1, bad)

let summary program =
  let functions = ref 0 and counts = ref (0, 0) in
  List.iter
    (function
      | Fn f ->
          incr functions;
          counts := List.fold_left count_stmt !counts f.body
      | Stmt s -> counts := count_stmt !counts s)
    program;
  let total, bad = !counts in
  let plural n word =
    Printf.sprintf "%d %s%s" n word (if n = 1 then "" else "s")
  in
  Printf.sprintf "parsed: %s, %s%s"
    (plural !functions "function")
    (plural total "statement")
    (if bad = 0 then "" else Printf.sprintf " (%d recovered)" bad)

(* ---- The gallery: a title and a program for each kind of mistake ---- *)

let valid =
  {|// Fibonacci, the slow way.
fn fib(n) {
  if n < 2 {
    return n;
  } else if n == 2 {
    return 1;
  } else {
    return fib(n - 1) + fib(n - 2);
  }
}

fn greet(name, excited) {
  let punctuation = "!";
  if !excited { punctuation = "."; }
  print("hello, " + name + punctuation);
}

let i = 0;
while i < 10 && !done(i) {
  print(fib(i) * -2 + 1);
  i = i + 1;
}
greet("world", true);
|}

let gallery =
  [
    ("a valid program", valid);
    ( "missing semicolon",
      {|fn area(w, h) {
  let result = w * h
  return result;
}
|} );
    ( "unclosed brace at the end",
      {|fn countdown(n) {
  while n > 0 {
    print(n);
    n = n - 1;
  return n;
}
|}
    );
    ( "several broken statements in one function",
      {|fn main() {
  let x = ;
  print(x);
  y = (1 + 2;
  print(x, );
  return x
}
print(main());
|}
    );
    ( "an error inside nested blocks",
      {|fn drain(n) {
  while n > 0 {
    if n % 2 == 0 {
      n = n - ;
    }
    n = n - 1;
  }
  return n;
}
|}
    );
    ( "a broken function header",
      {|fn drain(n m) {
  return n +;
}
fn next(n) { return n + 1; }
|} );
    ( "reserved word as a name",
      {|fn run() {
  let while = 10;
  return while;
}
|} );
    ( "chained comparison",
      {|fn check(low, x, high) {
  return low < x < high;
}
|} );
    ( "duplicate parameter",
      {|fn clamp(value, low, high, low) {
  return value;
}
|} );
    ("bad character", {|let total = 10 # 3;
print(total);
|});
    ("stray closing brace", {|fn one() {
  return 1;
}
}
print(one());
|});
  ]
