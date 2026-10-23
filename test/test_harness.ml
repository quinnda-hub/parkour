(* Exists only to prove the harness and both test tiers work. *)
let () =
  Parkour_test_support.Prop.run "harness"
    [
      Parkour_test_support.Prop.test ~name:"list rev involution"
        ~print:QCheck2.Print.(list int)
        QCheck2.Gen.(list int)
        (fun l -> List.rev (List.rev l) = l);
    ]
