open Parkour

type seq_style = By_bind | By_both | By_ap

type t =
  | Char of char
  | Str of string
  | Satisfy of string * int
  | Any
  | Eof
  | Seq of seq_style * t * t
  | Choice of t list
  | Alt of t * t
  | Many of t
  | Many1 of t
  | Opt of t
  | Sep_by of t * t
  | Sep_end_by of t * t
  | Many_till of t * t
  | Count of int * t
  | Label of string * t
  | Hidden of t
  | Fail of string
  | Cut
  | Attempt of t
  | Lookahead of t
  | Not_followed_by of t
  | Take_while of string option * int
  | Take_while1 of string * int

let mem mask c =
  c >= 'a' && c <= 'c' && mask land (1 lsl (Char.code c - 97)) <> 0

let rec to_string = function
  | Char c -> Printf.sprintf "Char %C" c
  | Str s -> Printf.sprintf "Str %S" s
  | Satisfy (l, m) -> Printf.sprintf "Satisfy(%s,%d)" l m
  | Any -> "Any"
  | Eof -> "Eof"
  | Seq (st, a, b) ->
      bin
        (match st with
        | By_bind -> "Seq_bind"
        | By_both -> "Seq_both"
        | By_ap -> "Seq_ap")
        a b
  | Choice l ->
      Printf.sprintf "Choice[%s]" (String.concat ";" (List.map to_string l))
  | Alt (a, b) -> bin "Alt" a b
  | Many g -> un "Many" g
  | Many1 g -> un "Many1" g
  | Opt g -> un "Opt" g
  | Sep_by (a, b) -> bin "Sep_by" a b
  | Sep_end_by (a, b) -> bin "Sep_end_by" a b
  | Many_till (a, b) -> bin "Many_till" a b
  | Count (n, g) -> Printf.sprintf "Count(%d,%s)" n (to_string g)
  | Label (l, g) -> Printf.sprintf "Label(%s,%s)" l (to_string g)
  | Hidden g -> un "Hidden" g
  | Fail m -> Printf.sprintf "Fail %S" m
  | Cut -> "Cut"
  | Attempt g -> un "Attempt" g
  | Lookahead g -> un "Lookahead" g
  | Not_followed_by g -> un "Not_followed_by" g
  | Take_while (l, m) ->
      Printf.sprintf "Take_while(%s,%d)" (Option.value l ~default:"-") m
  | Take_while1 (l, m) -> Printf.sprintf "Take_while1(%s,%d)" l m

and bin n a b = Printf.sprintf "%s(%s,%s)" n (to_string a) (to_string b)
and un n g = Printf.sprintf "%s(%s)" n (to_string g)

(* Generation *)

let gen_input =
  QCheck2.Gen.(string_size ~gen:(oneof_list [ 'a'; 'b'; 'c' ]) (int_bound 8))

let gen ~cuts =
  let open QCheck2.Gen in
  let label = oneof_list [ "x"; "y"; "z" ] in
  let mask = int_range 0 7 in
  let style = oneof_list [ By_bind; By_both; By_ap ] in
  let leaf =
    oneof_weighted
      ([
         (4, map (fun c -> Char c) (oneof_list [ 'a'; 'b'; 'c' ]));
         (2, map (fun s -> Str s) (oneof_list [ "ab"; "ba"; "abc"; "aa"; "bc" ]));
         (2, map2 (fun l m -> Satisfy (l, m)) label mask);
         (1, return Any);
         (1, return Eof);
         (1, map (fun m -> Fail m) (oneof_list [ "m1"; "m2" ]));
         (2, map2 (fun l m -> Take_while (l, m)) (option label) mask);
         (2, map2 (fun l m -> Take_while1 (l, m)) label mask);
       ]
      @ if cuts then [ (3, return Cut) ] else [])
  in
  let node self n =
    let sub = self (n / 2) and next = self (n - 1) in
    oneof_weighted
      [
        (3, leaf);
        (5, map3 (fun st a b -> Seq (st, a, b)) style sub sub);
        (1, map (fun l -> Choice l) (list_size (int_bound 4) sub));
        (5, map2 (fun a b -> Alt (a, b)) sub sub);
        (2, map (fun g -> Many g) next);
        (1, map (fun g -> Many1 g) next);
        (1, map (fun g -> Opt g) next);
        (1, map2 (fun a b -> Sep_by (a, b)) sub sub);
        (1, map2 (fun a b -> Sep_end_by (a, b)) sub sub);
        (1, map2 (fun a b -> Many_till (a, b)) sub sub);
        (1, map2 (fun n g -> Count (n, g)) (int_range 0 3) next);
        (1, map2 (fun l g -> Label (l, g)) label next);
        (1, map (fun g -> Hidden g) next);
        (1, map (fun g -> Attempt g) next);
        (1, map (fun g -> Lookahead g) next);
        (1, map (fun g -> Not_followed_by g) next);
      ]
  in
  sized_size (int_bound 8)
    (fix (fun self n -> if n <= 0 then leaf else node self n))

(* Compilation *)

let concat = String.concat ""

let rec compile g : string Parkour.t =
  let open Parkour in
  match g with
  | Char c -> map (String.make 1) (char c)
  | Str s -> string s
  | Satisfy (l, m) -> map (String.make 1) (satisfy l (mem m))
  | Any -> map (String.make 1) any_char
  | Eof -> map (fun () -> "") eof
  | Seq (By_bind, a, b) ->
      compile a >>= fun x ->
      compile b >>| fun y -> x ^ y
  | Seq (By_both, a, b) ->
      map (fun (x, y) -> x ^ y) (both (compile a) (compile b))
  | Seq (By_ap, a, b) -> map ( ^ ) (compile a) <*> compile b
  | Choice l -> choice (List.map compile l)
  | Alt (a, b) -> compile a <|> compile b
  | Many g -> map concat (many (compile g))
  | Many1 g -> map concat (many1 (compile g))
  | Opt g -> option "" (compile g)
  | Sep_by (sep, g) -> map concat (sep_by ~sep:(compile sep) (compile g))
  | Sep_end_by (sep, g) ->
      map concat (sep_end_by ~sep:(compile sep) (compile g))
  | Many_till (g, stop) -> map concat (many_till (compile g) (compile stop))
  | Count (n, g) -> map concat (count n (compile g))
  | Label (l, g) -> label l (compile g)
  | Hidden g -> hidden (compile g)
  | Fail m -> fail m
  | Cut -> map (fun () -> "") cut
  | Attempt g -> attempt (compile g)
  | Lookahead g -> lookahead (compile g)
  | Not_followed_by g -> map (fun () -> "") (not_followed_by (compile g))
  | Take_while (label, m) -> take_while ?label (mem m)
  | Take_while1 (l, m) -> take_while1 l (mem m)

let children = function
  | Char _ | Str _ | Satisfy _ | Any | Eof | Fail _ | Cut | Take_while _
  | Take_while1 _ ->
      []
  | Alt (a, b) | Sep_by (a, b) | Sep_end_by (a, b) | Many_till (a, b) ->
      [ a; b ]
  | Seq (_, a, b) -> [ a; b ]
  | Choice l -> l
  | Many g
  | Many1 g
  | Opt g
  | Count (_, g)
  | Label (_, g)
  | Hidden g
  | Attempt g
  | Lookahead g
  | Not_followed_by g ->
      [ g ]

let rec erase_cuts = function
  | Cut -> Count (0, Any)
  | ( Char _ | Str _ | Satisfy _ | Any | Eof | Fail _ | Take_while _
    | Take_while1 _ ) as g ->
      g
  | Seq (st, a, b) -> Seq (st, erase_cuts a, erase_cuts b)
  | Choice l -> Choice (List.map erase_cuts l)
  | Alt (a, b) -> Alt (erase_cuts a, erase_cuts b)
  | Sep_by (a, b) -> Sep_by (erase_cuts a, erase_cuts b)
  | Sep_end_by (a, b) -> Sep_end_by (erase_cuts a, erase_cuts b)
  | Many_till (a, b) -> Many_till (erase_cuts a, erase_cuts b)
  | Many g -> Many (erase_cuts g)
  | Many1 g -> Many1 (erase_cuts g)
  | Opt g -> Opt (erase_cuts g)
  | Count (n, g) -> Count (n, erase_cuts g)
  | Label (l, g) -> Label (l, erase_cuts g)
  | Hidden g -> Hidden (erase_cuts g)
  | Attempt g -> Attempt (erase_cuts g)
  | Lookahead g -> Lookahead (erase_cuts g)
  | Not_followed_by g -> Not_followed_by (erase_cuts g)

(* The oracle: a naive recursive interpreter. Every primitive failure is
   appended to a global log; the log is trimmed where the contract says
   failures are discarded. *)

module Oracle = struct
  type failure = {
    pos : int;
    expected : Expected.t list;
    custom : string option;
    unexpected : int option;
  }

  type outcome = Ok of string * int | Error of failure

  type entry = {
    at : int;
    items : Expected.t list;
    msg : string option;
    unexp : int option;
  }

  (* result of one step: success (value, position, committed) or failure
     (committed) *)
  type res = P of string * int * bool | F of bool

  let rec take n l =
    if n <= 0 then []
    else match l with [] -> [] | x :: r -> x :: take (n - 1) r

  let interpret g s ~with_eof =
    let n = String.length s in
    let log : entry list ref = ref [] in
    let record at items =
      log := { at; items; msg = None; unexp = None } :: !log
    in
    let record_custom at m =
      log := { at; items = []; msg = Some m; unexp = None } :: !log
    in
    let zero_width () = raise (Grammar_error "oracle: zero-width repetition") in
    let relabel items since pos =
      let fresh = List.length !log - since in
      log :=
        List.mapi
          (fun i e ->
            if i < fresh && e.at = pos && e.msg = None then
              { e with items; unexp = None }
            else e)
          !log
    in
    let rec go g pos =
      match g with
      | Char c ->
          if pos < n && s.[pos] = c then P (String.make 1 c, pos + 1, false)
          else (
            record pos [ Expected.Literal (String.make 1 c) ];
            F false)
      | Str t ->
          let l = String.length t in
          if pos + l <= n && String.sub s pos l = t then P (t, pos + l, false)
          else (
            record pos [ Expected.Literal t ];
            F false)
      | Satisfy (l, m) ->
          if pos < n && mem m s.[pos] then
            P (String.make 1 s.[pos], pos + 1, false)
          else (
            record pos [ Expected.Label l ];
            F false)
      | Any ->
          if pos < n then P (String.make 1 s.[pos], pos + 1, false)
          else (
            record pos [];
            F false)
      | Eof ->
          if pos >= n then P ("", pos, false)
          else (
            record pos [ Expected.End_of_input ];
            F false)
      | Seq (_, a, b) -> (
          match go a pos with
          | F c -> F c
          | P (va, pa, ca) -> (
              match go b pa with
              | F cb -> F (ca || cb)
              | P (vb, pb, cb) -> P (va ^ vb, pb, ca || cb)))
      | Alt (a, b) -> (
          let before = !log in
          match go a pos with
          | P _ as r -> r
          | F true -> F true
          | F false -> (
              let after_a = !log in
              match go b pos with
              | (P (_, _, false) | F false) as r -> r
              | r ->
                  (* the right side committed: forget the left side's failures *)
                  let mine =
                    take (List.length !log - List.length after_a) !log
                  in
                  log := mine @ before;
                  r))
      | Many g -> loop g pos "" false
      | Many1 g -> go (Seq (By_bind, g, Many g)) pos
      | Choice [] ->
          record pos [];
          F false
      | Choice (h :: rest) ->
          go (List.fold_left (fun acc x -> Alt (acc, x)) h rest) pos
      | Opt g -> go (Alt (g, Count (0, Any))) pos
      | Sep_by (sep, g) -> (
          let first =
            match go g pos with
            | F c -> F c
            | P (v, p, c) -> (
                let it p =
                  match go sep p with
                  | F c -> F c
                  | P (_, p1, c1) -> (
                      match go g p1 with
                      | F c2 -> F (c1 || c2)
                      | P (v2, p2, c2) -> P (v2, p2, c1 || c2))
                in
                let rec rest p acc cut =
                  match it p with
                  | P (v2, p2, c2) ->
                      if p2 = p then zero_width ();
                      rest p2 (acc ^ v2) (cut || c2)
                  | F true -> F true
                  | F false -> P (acc, p, cut)
                in
                match rest p v c with
                | P (v, p, c') -> P (v, p, c || c')
                | F _ -> F true)
          in
          match first with F false -> P ("", pos, false) | r -> r)
      | Sep_end_by (sep, g) -> (
          let rec rest p acc cut =
            match go sep p with
            | F true -> F true
            | F false -> P (acc, p, cut)
            | P (_, p1, c1) -> (
                let cut = cut || c1 in
                match go g p1 with
                | P (v, p2, c2) ->
                    if p2 = p then zero_width ();
                    rest p2 (acc ^ v) (cut || c2)
                | F true -> F true
                | F false -> P (acc, p1, cut))
          in
          let first =
            match go g pos with
            | F c -> F c
            | P (v, p, c) -> (
                match rest p v c with
                | P (v, p, c') -> P (v, p, c || c')
                | F _ -> F true)
          in
          match first with F false -> P ("", pos, false) | r -> r)
      | Many_till (g, stop) ->
          let rec step p acc cut =
            let before = !log in
            match go stop p with
            | P (_, p', c) -> P (acc, p', cut || c)
            | F true -> F true
            | F false -> (
                let after_stop = !log in
                let drop_stop () =
                  let mine =
                    take (List.length !log - List.length after_stop) !log
                  in
                  log := mine @ before
                in
                match go g p with
                | P (v, p2, c) ->
                    if p2 = p then zero_width ();
                    if c then drop_stop ();
                    step p2 (acc ^ v) (cut || c)
                | F true ->
                    drop_stop ();
                    F true
                | F false -> F cut)
          in
          step pos "" false
      | Count (k, g) ->
          let rec rep i p acc cut =
            if i = 0 then P (acc, p, cut)
            else
              match go g p with
              | F c -> F (cut || c)
              | P (v, p2, c) -> rep (i - 1) p2 (acc ^ v) (cut || c)
          in
          rep k pos "" false
      | Label (l, g) -> labelled [ Expected.Label l ] g pos
      | Hidden g ->
          let before = !log in
          let since = List.length before in
          let r = go g pos in
          (match r with
          | P _ -> log := before
          | F _ ->
              let fresh = List.length !log - since in
              log :=
                List.mapi
                  (fun i e ->
                    if i < fresh && e.at = pos && e.msg = None && e.unexp = None
                    then { e with items = [] }
                    else e)
                  !log);
          r
      | Fail m ->
          record_custom pos m;
          F false
      | Cut -> P ("", pos, true)
      | Attempt g -> (
          match go g pos with P (v, p, _) -> P (v, p, false) | F _ -> F false)
      | Lookahead g -> (
          let before = !log in
          match go g pos with
          | P (v, _, _) ->
              log := before;
              P (v, pos, false)
          | F _ -> F false)
      | Not_followed_by g -> (
          let before = !log in
          let r = go g pos in
          log := before;
          match r with
          | F _ -> P ("", pos, false)
          | P (_, stop, _) ->
              log :=
                { at = pos; items = []; msg = None; unexp = Some stop } :: !log;
              F false)
      | Take_while (l, m) ->
          let stop = scan m pos in
          Option.iter (fun l -> record stop [ Expected.Label l ]) l;
          P (String.sub s pos (stop - pos), stop, false)
      | Take_while1 (l, m) ->
          let stop = scan m pos in
          if stop = pos then (
            record pos [ Expected.Label l ];
            F false)
          else P (String.sub s pos (stop - pos), stop, false)
    and scan m p = if p < n && mem m s.[p] then scan m (p + 1) else p
    and loop g pos acc cut =
      match go g pos with
      | P (v, p, c) ->
          if p = pos then zero_width ();
          loop g p (acc ^ v) (cut || c)
      | F true -> F true
      | F false -> P (acc, pos, cut)
    and labelled items g pos =
      let since = List.length !log in
      let r = go g pos in
      relabel items since pos;
      r
    in
    let top = if with_eof then Seq (By_bind, g, Eof) else g in
    match go top 0 with
    | P (v, p, _) -> Ok (v, p)
    | F _ ->
        let entries = List.rev !log in
        let maxpos = List.fold_left (fun m e -> max m e.at) 0 entries in
        let here = List.filter (fun e -> e.at = maxpos) entries in
        let custom = List.find_map (fun e -> e.msg) here in
        let items =
          List.sort_uniq Expected.compare
            (List.concat_map (fun e -> e.items) here)
        in
        let unexpected =
          if custom <> None || items <> [] then None
          else List.find_map (fun e -> e.unexp) here
        in
        Error
          {
            pos = maxpos;
            expected = (if custom = None then items else []);
            custom;
            unexpected;
          }

  let run_prefix g s = interpret g s ~with_eof:false
  let run g s = interpret g s ~with_eof:true
end
