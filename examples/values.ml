(* Small one-line value parsers, as used to validate command-line arguments or
   config values: a semantic version, an ISO date and a duration.

   Run it on values:        dune exec examples/values.exe -- semver:1.2.3 date:2023-02-30 duration:1h30m
   Or on built-in samples:  dune exec examples/values.exe
   Print the error gallery: dune exec examples/values.exe -- --gallery

   What they show: [validate] turns a semantic check into an error that
   underlines exactly the offending part (leading zeros, month 13, February
   30th); [spanned] and [fail ~span ~labels ~notes] build errors with secondary
   labels and notes when the library's own messages are not enough (a
   duplicated or misordered duration unit). *)

open Parkour

(* ---- Semantic versions: MAJOR.MINOR.PATCH(-prerelease)?(+build)? ---- *)

type semver = {
  major : int;
  minor : int;
  patch : int;
  pre : string list;
  build : string list;
}

let all_digits s = String.for_all is_digit s

(* semver.org: numeric identifiers have no leading zeros. [validate] reports
   the error on everything the digits parser consumed, so the whole number is
   underlined. *)
let no_leading_zeros s =
  if String.length s > 1 && s.[0] = '0' then
    Error "leading zeros are not allowed"
  else Ok s

let number_part =
  take_while1 "number" is_digit
  |> validate no_leading_zeros
  |> validate (fun s ->
      match int_of_string_opt s with
      | Some n -> Ok n
      | None -> Error "number out of range")

(* Prerelease and build identifiers are [0-9A-Za-z-]+, dot-separated. Only
   all-digit prerelease identifiers must avoid leading zeros ("01" is
   forbidden but "0a" is fine); build identifiers have no such rule. *)
let identifier = take_while1 "identifier" (fun c -> is_alnum c || c = '-')

let pre_identifier =
  identifier
  |> validate (fun s -> if all_digits s then no_leading_zeros s else Ok s)

(* [cut] after '-' and '+': once we see one, an identifier must follow. *)
let dotted ident = sep_by1 ~sep:(char '.') ident

let semver =
  let+ major = number_part <* char '.'
  and+ minor = number_part <* char '.'
  and+ patch = number_part
  and+ pre = option [] (char '-' *> cut *> dotted pre_identifier)
  and+ build = option [] (char '+' *> cut *> dotted identifier) in
  { major; minor; patch; pre; build }

let show_semver v =
  Printf.sprintf "%d.%d.%d%s%s" v.major v.minor v.patch
    (if v.pre = [] then "" else " pre=" ^ String.concat "." v.pre)
    (if v.build = [] then "" else " build=" ^ String.concat "." v.build)

(* ---- ISO dates: YYYY-MM-DD ---- *)

type date = { year : int; month : int; day : int }

let month_names =
  [|
    "January";
    "February";
    "March";
    "April";
    "May";
    "June";
    "July";
    "August";
    "September";
    "October";
    "November";
    "December";
  |]

let is_leap y = (y mod 4 = 0 && y mod 100 <> 0) || y mod 400 = 0

let days_in_month year month =
  match month with
  | 2 -> if is_leap year then 29 else 28
  | 4 | 6 | 9 | 11 -> 30
  | _ -> 31

(* [digits name n]: exactly [n] digits. Reading all the digits first and then
   checking the count underlines the whole wrong-length number. *)
let digits name n =
  take_while1 "digit" is_digit
  |> validate (fun s ->
      if String.length s = n then Ok (int_of_string s)
      else Error (Printf.sprintf "%s must have exactly %d digits" name n))

let date =
  let* year = digits "year" 4 <* char '-' in
  let* month =
    digits "month" 2
    |> validate (fun m ->
        if m >= 1 && m <= 12 then Ok m
        else Error "month must be between 01 and 12")
  in
  let+ day =
    char '-'
    *> (digits "day" 2
       |> validate (fun d ->
           if d >= 1 && d <= days_in_month year month then Ok d
           else
             Error
               (Printf.sprintf "day %d is out of range for %s %d" d
                  month_names.(month - 1)
                  year)))
  in
  { year; month; day }

(* ---- Durations: 1h30m, 90s, 2d4h ---- *)

type unit_ = { name : char; seconds : int }

let units =
  [
    { name = 'd'; seconds = 86400 };
    { name = 'h'; seconds = 3600 };
    { name = 'm'; seconds = 60 };
    { name = 's'; seconds = 1 };
  ]

(* One component, e.g. "30m", with its span so later checks can point at it. *)
let component =
  let amount =
    natural
    |> validate (fun n ->
        if n > 0 then Ok n else Error "value must be positive")
  in
  spanned
    (let+ n = amount and+ c = one_of "dhms" in
     (n, List.find (fun u -> u.name = c) units))

(* The checks need the whole list: a duplicate points back at the first
   occurrence, a misordered unit explains the order. The failure underlines
   the offending component. *)
let check_order comps =
  let rec go seen = function
    | [] -> None
    | (((_, u), span) as c) :: rest -> (
        match List.find_opt (fun ((_, v), _) -> v.name = u.name) seen with
        | Some (_, first) ->
            Some
              (fail ~span
                 ~labels:[ (first, "first given here") ]
                 (Printf.sprintf "unit '%c' is given more than once" u.name))
        | None -> (
            match seen with
            | ((_, prev), _) :: _ when prev.seconds <= u.seconds ->
                Some
                  (fail ~span
                     ~notes:
                       [ "write units from largest to smallest: d, h, m, s" ]
                     (Printf.sprintf "unit '%c' must come before '%c'" u.name
                        prev.name))
            | _ -> go (c :: seen) rest))
  in
  go [] comps

(* The value is the total number of seconds. *)
let duration =
  let* comps = many1 component in
  match check_order comps with
  | Some failure -> failure
  | None ->
      return (List.fold_left (fun t ((n, u), _) -> t + (n * u.seconds)) 0 comps)

(* ---- Running ---- *)

let show_date d = Printf.sprintf "%04d-%02d-%02d" d.year d.month d.day
let show_duration n = Printf.sprintf "%d seconds" n

(* Parse [input] with [p] and print the value or the rendered diagnostics. *)
let try_parse ?(name = "gallery.txt") p show input =
  match parse p input with
  | Ok v -> Printf.printf "ok: %s\n" (show v)
  | Error diagnostics ->
      let source = Source.make ~name input in
      List.iter (fun d -> print_string (Diagnostic.render source d)) diagnostics

(* Each gallery case is a title and the input for one of the parsers. *)
let semver_case title input =
  (title, fun () -> try_parse semver show_semver input)

let date_case title input = (title, fun () -> try_parse date show_date input)

let duration_case title input =
  (title, fun () -> try_parse duration show_duration input)

let gallery =
  [
    semver_case "semver: valid" "1.4.0-rc.1+build.5";
    semver_case "semver: leading zero in the minor number" "1.04.0";
    semver_case "semver: leading zero in a numeric prerelease identifier"
      "1.4.0-rc.01";
    semver_case "semver: missing patch number" "1.4";
    semver_case "semver: empty prerelease identifier" "1.4.0-rc..1";
    semver_case "semver: invalid character in the build metadata"
      "1.4.0+build_5";
    date_case "date: valid (leap day)" "2024-02-29";
    date_case "date: month out of range" "2023-13-01";
    date_case "date: day out of range for February" "2023-02-29";
    date_case "date: day out of range for a 30-day month" "2023-04-31";
    date_case "date: month with one digit" "2023-1-05";
    date_case "date: wrong separator" "2023/01/05";
    duration_case "duration: valid" "2d4h30m";
    duration_case "duration: duplicate unit" "1h30m15m";
    duration_case "duration: units out of order" "30m1h";
    duration_case "duration: zero amount" "0s";
    duration_case "duration: unit missing" "1h30";
    duration_case "duration: unknown unit" "2w";
  ]

let run_gallery () =
  List.iter
    (fun (title, run) ->
      Printf.printf "=== %s ===\n" title;
      run ())
    gallery

(* Arguments look like [semver:1.2.3], [date:2023-02-30], [duration:1h30m]. *)
let run_arg arg =
  Printf.printf "%s\n" arg;
  let name = "<arg>" in
  match String.index_opt arg ':' with
  | None ->
      prerr_endline "expected KIND:VALUE with KIND semver, date or duration"
  | Some i -> (
      let value = String.sub arg (i + 1) (String.length arg - i - 1) in
      match String.sub arg 0 i with
      | "semver" -> try_parse ~name semver show_semver value
      | "date" -> try_parse ~name date show_date value
      | "duration" -> try_parse ~name duration show_duration value
      | kind -> Printf.eprintf "unknown kind %S\n" kind)

let () =
  match List.tl (Array.to_list Sys.argv) with
  | [ "--gallery" ] -> run_gallery ()
  | [] ->
      List.iter run_arg
        [ "semver:1.0.0-alpha"; "date:2023-02-30"; "duration:1h30m" ]
  | args -> List.iter run_arg args
