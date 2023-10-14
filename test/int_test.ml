open Test_utils
open Stt

let ( !! ) =
  List.fold_left
    (fun acc (i, j) -> Int.(cup acc (range (Z.of_int i) (Z.of_int j))))
    Int.empty

let ( !. ) =
  List.fold_left (fun acc i -> Int.(cup acc (singleton (Z.of_int i)))) Int.empty

let l1 = !![ 65, 100; 255, 300; 450, 900 ]
let l2 = !![ 70, 99; 100, 255; 350, 400; 402, 800 ]
let l1_cup_l2 = !![ 65, 300; 350, 400; 402, 900 ]
let l1_cap_l2 = !![ 70, 99; 100, 100; 255, 255; 450, 800 ]
let l1_diff_l2 = !![ 65, 69; 256, 300; 801, 900 ]
let l2_diff_l1 = !![ 101, 254; 350, 400; 402, 449 ]
let half_ints = List.init 20 Fun.id
let odd = List.map (fun x -> (x * 2) + 1) half_ints
let even = List.map (fun x -> x * 2) half_ints
let ints = List.init 40 Fun.id

let () =
  let check = check (module Int) in
  let open Int in
  run "Int"
    [
      ( "union",
        [
          check any (cup any empty);
          check any (cup any !.[ 42 ]);
          check any (cup (left Z.minus_one) (right Z.zero));
          check l1_cup_l2 (cup l1 l2);
          check !.ints (cup !.even !.odd);
        ] );
      ( "inter",
        [
          check any (cap any any);
          check !![ 10, 100 ] (cap (left (Z.of_int 100)) (right (Z.of_int 10)));
          check empty (cap (right Z.zero) (left Z.minus_one));
          check !.[ -1; 0; 1 ] (cap (left Z.one) (right Z.minus_one));
          check l1_cap_l2 (cap l1 l2);
          check empty (cap !.even !.odd);
        ] );
      ( "diff",
        [
          check empty (diff empty any);
          check any (diff any empty);
          check empty (diff any any);
          check (cup (left Z.minus_one) (right Z.one)) (diff any !.[ 0 ]);
          check l1_diff_l2 (diff l1 l2);
          check l2_diff_l1 (diff l2 l1);
          check !.odd (diff !.ints !.even);
        ] );
    ]
