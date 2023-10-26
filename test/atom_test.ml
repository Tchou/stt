open Test_utils
open Stt

let ( !$ ) =
  List.fold_left (fun acc i -> Atom.(cup acc (singleton (Base.Hstring.make i)))) Atom.empty

let l1 = !$[ "A"; "B"; "C"; "D" ]
let l2 = !$[ "A"; "B"; "E"; "F"; "G" ]
let l1_cup_l2 = !$[ "A"; "B"; "C"; "D"; "E"; "F"; "G" ]
let l1_cap_l2 = !$[ "A"; "B" ]
let l1_diff_l2 = !$[ "C"; "D" ]
let neg_l1 = Atom.neg l1
let neg_l2 = Atom.neg l2
let neg_l1_cup_neg_l2 = Atom.neg l1_cap_l2
let neg_l1_cap_neg_l2 = Atom.neg l1_cup_l2
let l1_cup_neg_l2 = Atom.neg !$[ "E"; "F"; "G" ]

let () =
  let checkb = check T.bool in
  let check = check (module Atom) in
  let open Atom in
  run "Atom"
    [
      ( "is_finite",
        [
          checkb true (is_finite empty);
          checkb false (is_finite any);
          checkb true (is_finite l1);
          checkb false (is_finite neg_l1);
        ] );
      ( "neg",
        [
          check any (neg empty);
          check empty (neg any);
          check l1 (neg (neg l1));
          check (neg l1) (neg l1);
          (* to print *)
        ] );
      ( "union",
        [
          check empty (cup empty empty);
          check any (cup any empty);
          check any (cup any !$[ "A" ]);
          check l1_cup_l2 (cup l1 l2);
          check neg_l1_cup_neg_l2 (cup neg_l1 neg_l2);
          check l1_cup_neg_l2 (cup l1 neg_l2);
        ] );
      ( "inter",
        [
          check any (cap any any);
          check empty (cap any empty);
          check empty (cap !$[ "A" ] !$[ "B" ]);
          check !$[ "A" ] (cap any !$[ "A" ]);
          check l1_cap_l2 (cap l1 l2);
          check neg_l1_cap_neg_l2 (cap neg_l1 neg_l2);
          check l1_diff_l2 (cap l1 neg_l2);
        ] );
    ]
