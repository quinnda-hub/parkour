(* Properties of the operator-expression helpers: [expression] against an
   independent recursive Pratt reference, printing/parsing roundtrips, error
   expectations, the laws relating [chainl1]/[chainr1]/[between] to their
   definitions, and stack safety. *)

module P = Parkour
module D = P.Diagnostic
module Prop = Parkour_test_support.Prop
open QCheck2

(* Operator tables *)

type kind = Prefix_k | Postfix_k | Infixl_k | Infixr_k | Infixn_k
type op = { kind : kind; sym : char }
type table = op list list (* tightest level first *)

type ast =
  | X
  | Pre of char * ast
  | Post of char * ast
  | Bin of char * ast * ast

let rec show_ast = function
  | X -> "x"
  | Pre (c, a) -> Printf.sprintf "(%c %s)" c (show_ast a)
  | Post (c, a) -> Printf.sprintf "(%s %c)" (show_ast a) c
  | Bin (c, a, b) -> Printf.sprintf "(%s %c %s)" (show_ast a) c (show_ast b)

let show_kind = function
  | Prefix_k -> "prefix"
  | Postfix_k -> "postfix"
  | Infixl_k -> "infixl"
  | Infixr_k -> "infixr"
  | Infixn_k -> "infix"

let show_table (t : table) =
  String.concat " | "
    (List.map
       (fun l ->
         String.concat " "
           (List.map
              (fun o -> Printf.sprintf "%s %c" (show_kind o.kind) o.sym)
              l))
       t)

let pool = "+-*/^<=!?~%&"

(* Infix and postfix operators get distinct symbols; prefix operators are
   distinct among themselves but may reuse any symbol, as unary minus does. *)
let assign levels : table =
  let next = ref 0 and used_prefix = ref [] in
  let sym_prefix r =
    let rec find i =
      let c = pool.[i mod String.length pool] in
      if List.mem c !used_prefix then find (i + 1) else c
    in
    let c = find r in
    used_prefix := c :: !used_prefix;
    c
  in
  List.map
    (fun (assoc, entries) ->
      List.map
        (fun (shape, r) ->
          match shape with
          | `Pre -> { kind = Prefix_k; sym = sym_prefix r }
          | `Post | `In ->
              let sym = pool.[!next] in
              incr next;
              { kind = (if shape = `Post then Postfix_k else assoc); sym })
        entries)
    levels

let gen_table : table Gen.t =
  let open Gen in
  let level =
    pair
      (oneof_list [ Infixl_k; Infixr_k; Infixn_k ])
      (list_size (int_range 1 3)
         (pair (oneof_list [ `Pre; `Post; `In; `In ]) (int_range 0 11)))
  in
  map assign (list_size (int_range 1 4) level)

let table_ops (t : table) = List.concat t

let syms kind t =
  List.filter_map
    (fun o -> if o.kind = kind then Some o.sym else None)
    (table_ops t)

let infix_syms t =
  List.filter_map
    (fun o ->
      match o.kind with
      | Infixl_k | Infixr_k | Infixn_k -> Some o.sym
      | Prefix_k | Postfix_k -> None)
    (table_ops t)

(* Parkour's side *)

let atom = P.(char 'x' >>| fun _ -> X)

let to_parkour (t : table) : ast P.operator list list =
  List.map
    (List.map (fun o ->
         let c = o.sym in
         let tok = P.char c in
         match o.kind with
         | Prefix_k -> P.Prefix P.(tok >>| fun _ a -> Pre (c, a))
         | Postfix_k -> P.Postfix P.(tok >>| fun _ a -> Post (c, a))
         | Infixl_k -> P.Infix_left P.(tok >>| fun _ a b -> Bin (c, a, b))
         | Infixr_k -> P.Infix_right P.(tok >>| fun _ a b -> Bin (c, a, b))
         | Infixn_k -> P.Infix_none P.(tok >>| fun _ a b -> Bin (c, a, b))))
    t

(* The expression grammar the tests use, with parenthesised sub-expressions. *)
let parser_of (t : table) =
  P.fix (fun e ->
      P.expression P.(atom <|> between (char '(') (char ')') e) (to_parkour t))

type 'a r = V of 'a * int | E of D.t

let run p s =
  match P.parse_prefix p s with
  | Ok (v, pos) -> V (v, pos)
  | Error (d :: _) -> E d
  | Error [] -> failwith "no diagnostic"

(* An independent reference: a recursive Pratt parser over the same grammar
   (PLAN.md §3.7 binding powers). Where the library's parsing stops early
   (an operator whose operand fails, or a failed parenthesised term inside
   an operand) the flat operator sequence is simply cut there, which the
   reference does by retrying with a lower input limit. *)

exception Chained
exception Cut of int option
(* The operand of the iteration started by the operator at this position (None:
   none) did not parse. *)

let reference (t : table) s =
  let n = List.length t in
  let ops =
    List.concat (List.mapi (fun k l -> List.map (fun o -> (n - k, o)) l) t)
  in
  let prefix c =
    List.find_map
      (fun (i, o) -> if o.kind = Prefix_k && o.sym = c then Some i else None)
      ops
  in
  let operator c =
    List.find_opt (fun (_, o) -> o.kind <> Prefix_k && o.sym = c) ops
  in
  let rec sub start q lim =
    let rec attempt lim =
      match pratt lim q 0 start with
      | r -> r
      | exception Cut (Some p) when p >= start -> attempt p
      | exception Chained -> raise (Cut q)
    in
    attempt lim
  and operand lim q pos =
    if pos < lim && prefix s.[pos] <> None then
      let i = Option.get (prefix s.[pos]) in
      let a, pos' = pratt lim q ((6 * i) + 2) (pos + 1) in
      (Pre (s.[pos], a), pos')
    else if pos >= lim then raise (Cut q)
    else
      match s.[pos] with
      | 'x' -> (X, pos + 1)
      | '(' ->
          let a, pos' = sub (pos + 1) q lim in
          if pos' < lim && s.[pos'] = ')' then (a, pos' + 1) else raise (Cut q)
      | _ -> raise (Cut q)
  (* Whether an operand (prefix operators then a term) starts at [pos]. *)
  and operand_ok lim q pos =
    if pos >= lim then false
    else if prefix s.[pos] <> None then operand_ok lim q (pos + 1)
    else
      match s.[pos] with
      | 'x' -> true
      | '(' -> (
          match sub (pos + 1) (Some q) lim with
          | _, pos' -> pos' < lim && s.[pos'] = ')'
          | exception Cut _ -> false)
      | _ -> false
  and pratt lim q minbp pos =
    let lhs, pos = operand lim q pos in
    loop lim minbp lhs pos None
  and loop lim minbp lhs pos last_none =
    if pos >= lim then (lhs, pos)
    else
      match operator s.[pos] with
      | None -> (lhs, pos)
      | Some (i, { kind = Postfix_k; sym }) ->
          if (6 * i) + 4 > minbp then
            loop lim minbp (Post (sym, lhs)) (pos + 1) None
          else (lhs, pos)
      | Some (i, { kind; sym }) ->
          (* the iteration only exists if its operand parses *)
          if not (operand_ok lim pos (pos + 1)) then raise (Cut (Some pos));
          if 6 * i > minbp then (
            if last_none = Some i then raise Chained;
            let rbp = if kind = Infixr_k then (6 * i) - 1 else 6 * i in
            let rhs, pos' = pratt lim (Some pos) rbp (pos + 1) in
            loop lim minbp
              (Bin (sym, lhs, rhs))
              pos'
              (if kind = Infixn_k then Some i else None))
          else (lhs, pos)
  in
  match sub 0 None (String.length s) with
  | a, pos -> Some (a, pos)
  | exception Cut None -> None

(* Generators *)

(* Token strings for a table: operator-shaped trees printed without
   precedence-aware parentheses (random ones are added), so many are valid,
   some chain non-associative operators, some are malformed. *)
let gen_tokens (t : table) =
  let open Gen in
  let ops kind = syms kind t in
  let infix = infix_syms t in
  let pick = function
    | [] -> return None
    | l -> map Option.some (oneof_list l)
  in
  let rec go fuel =
    if fuel = 0 then return "x"
    else
      let sub = go (fuel - 1) in
      let un kind combine =
        pick (ops kind) >>= function
        | None -> return "x"
        | Some c -> map (combine (String.make 1 c)) sub
      in
      oneof_weighted
        [
          (2, return "x");
          (2, un Prefix_k (fun c a -> c ^ a));
          (2, un Postfix_k (fun c a -> a ^ c));
          ( 5,
            pick infix >>= function
            | None -> return "x"
            | Some c ->
                map2
                  (fun a b -> a ^ String.make 1 c ^ b)
                  (go (fuel / 2))
                  (go (fuel / 2)) );
          (1, map (fun a -> "(" ^ a ^ ")") sub);
        ]
  in
  let mutate s =
    oneof_weighted
      [
        (6, return s);
        ( 2,
          map
            (fun k -> String.sub s 0 (k * String.length s / 8))
            (int_range 0 8) );
        ( 2,
          map2
            (fun k c ->
              if s = "" then String.make 1 c
              else
                let b = Bytes.of_string s in
                Bytes.set b (k mod String.length s) c;
                Bytes.to_string b)
            (int_range 0 20)
            (oneof_list
               ([ 'x'; '('; ')'; '#' ] @ List.init 4 (String.get pool))) );
      ]
  in
  int_range 0 4 >>= go >>= mutate

let gen_case = Gen.(gen_table >>= fun t -> map (fun s -> (t, s)) (gen_tokens t))
let print_case (t, s) = Printf.sprintf "%s on %S" (show_table t) s

(* Random ASTs over a table's operators. *)
let gen_ast (t : table) =
  let open Gen in
  let ops kind = syms kind t in
  let infix = infix_syms t in
  let rec go fuel =
    if fuel = 0 then return X
    else
      let sub = go (fuel - 1) in
      let un kind make =
        match ops kind with [] -> return X | l -> map2 make (oneof_list l) sub
      in
      oneof_weighted
        [
          (2, return X);
          (2, un Prefix_k (fun c a -> Pre (c, a)));
          (2, un Postfix_k (fun c a -> Post (c, a)));
          ( 4,
            match infix with
            | [] -> return X
            | l ->
                map3
                  (fun c a b -> Bin (c, a, b))
                  (oneof_list l)
                  (go (fuel / 2))
                  (go (fuel / 2)) );
        ]
  in
  int_range 0 5 >>= go

let rec print_full = function
  | X -> "x"
  | Pre (c, a) -> Printf.sprintf "(%c%s)" c (print_full a)
  | Post (c, a) -> Printf.sprintf "(%s%c)" (print_full a) c
  | Bin (c, a, b) -> Printf.sprintf "(%s%c%s)" (print_full a) c (print_full b)

(* Properties *)

let agreement =
  Prop.test ~count:400
    ~name:"expression agrees with the recursive Pratt reference"
    ~print:print_case gen_case (fun (t, s) ->
      match (run (parser_of t) s, reference t s) with
      | V (a, pos), Some (a', pos') -> a = a' && pos = pos'
      | E _, None -> true
      | _ -> false)

let roundtrip =
  Prop.test ~name:"a fully parenthesised AST parses back to itself"
    ~print:(fun (t, a) -> show_table t ^ ": " ^ show_ast a)
    Gen.(gen_table >>= fun t -> map (fun a -> (t, a)) (gen_ast t))
    (fun (t, a) ->
      match P.parse (parser_of t) (print_full a) with
      | Ok a' -> a = a'
      | Error _ -> false)

(* What [char c] says it expects. *)
let literal c =
  match P.parse (P.char c) "#" with
  | Error (d :: _) -> D.expected d
  | _ -> failwith "char accepted '#'"

let subset l l' = List.for_all (fun e -> List.mem e l') l

let errors_expect_operators =
  Prop.test
    ~name:"after an operand, errors expect every postfix and infix operator"
    ~print:show_table gen_table (fun t ->
      let p = P.expression atom (to_parkour t) in
      match run P.(p <* eof) "x#" with
      | E d ->
          (D.span d).start = 1
          && subset
               (List.concat_map literal (syms Postfix_k t @ infix_syms t))
               (D.expected d)
      | V _ -> false)

let errors_expect_operands =
  Prop.test
    ~name:"after an infix operator, errors expect prefix operators and the term"
    ~print:show_table gen_table (fun t ->
      let p = P.expression atom (to_parkour t) in
      let expected = List.concat_map literal (syms Prefix_k t) @ literal 'x' in
      List.for_all
        (fun c ->
          match run P.(p <* eof) (Printf.sprintf "x%c#" c) with
          | E d -> (D.span d).start = 2 && subset expected (D.expected d)
          | V _ -> false)
        (infix_syms t))

let chained_error =
  Prop.test ~name:"a chained non-associative operator is reported at the second"
    ~print:show_table gen_table (fun t ->
      let t = t @ [ [ { kind = Infixn_k; sym = '@' } ] ] in
      let p = P.expression atom (to_parkour t) in
      List.for_all
        (fun c ->
          let s = Printf.sprintf "x%cx%cx" c c in
          match run p s with
          | E d ->
              D.message d = "operator cannot be chained"
              && D.span d = P.Span.make ~start:3 ~stop:4
          | V _ -> false)
        (List.filter_map
           (fun o -> if o.kind = Infixn_k then Some o.sym else None)
           (table_ops t)))

(* Laws *)

let obs = function
  | V (v, pos) -> `Ok (v, pos)
  | E d -> `Error (D.message d, D.span d, D.expected d)

let gen_input alphabet =
  Gen.string_size ~gen:(Gen.oneof_list alphabet) (Gen.int_range 0 8)

let leaf = P.(char 'x' >>| String.make 1)
let plus = P.(char '+' >>| fun _ a b -> "(" ^ a ^ "+" ^ b ^ ")")

let law name expected actual =
  Prop.test ~name ~print:(Printf.sprintf "%S")
    (gen_input [ 'x'; '+'; '#' ])
    (fun s -> obs (run expected s) = obs (run actual s))

let chainl1_law =
  law "chainl1 p op = expression p [[Infix_left op]]" (P.chainl1 leaf plus)
    (P.expression leaf [ [ P.Infix_left plus ] ])

let chainr1_law =
  law "chainr1 p op = expression p [[Infix_right op]]" (P.chainr1 leaf plus)
    (P.expression leaf [ [ P.Infix_right plus ] ])

let between_law =
  Prop.test ~name:"between o c p = o *> p <* c" ~print:(Printf.sprintf "%S")
    (gen_input [ 'x'; '('; ')'; '#' ])
    (fun s ->
      obs (run (P.between (P.char '(') (P.char ')') leaf) s)
      = obs (run P.(char '(' *> leaf <* char ')') s))

(* Robustness. 10^6 chains: see test_robustness.ml. *)

let robustness =
  Prop.test ~count:1 ~name:"very long chains parse without stack overflow"
    Gen.unit (fun () ->
      let n = 100_000 in
      let one = P.(char '1' >>| fun _ -> 1) in
      let add = P.(char '+' >>| fun _ a b -> a + b) in
      let inc c = P.(char c >>| fun _ a -> a + 1) in
      let chain = "1" ^ String.concat "" (List.init (n - 1) (fun _ -> "+1")) in
      let value p s = P.parse p s = Ok n in
      let value' p s = P.parse p s = Ok (n + 1) in
      value (P.expression one [ [ P.Infix_left add ] ]) chain
      && value (P.expression one [ [ P.Infix_right add ] ]) chain
      && value (P.expression one [ [ P.Infix_none add ] ]) "1+1" = false
      && value (P.chainl1 one add) chain
      && value (P.chainr1 one add) chain
      && value'
           (P.expression one [ [ P.Prefix (inc '-') ] ])
           (String.make n '-' ^ "1")
      && value'
           (P.expression one [ [ P.Postfix (inc '!') ] ])
           ("1" ^ String.make n '!')
      &&
      match P.expression one [ [ P.Infix_left add; P.Infix_right add ] ] with
      | _ -> false
      | exception Invalid_argument _ -> true)

let () =
  Prop.run "expr"
    [
      agreement;
      roundtrip;
      errors_expect_operators;
      errors_expect_operands;
      chained_error;
      chainl1_law;
      chainr1_law;
      between_law;
      robustness;
    ]
