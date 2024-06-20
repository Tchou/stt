open Utils
open Stt

let ( !! ) =
  List.fold_left
    (fun acc (i, j) -> Char.(cup acc (range (Uchar.of_int i) (Uchar.of_int j))))
    Char.empty

let ( !@ ) =
  List.fold_left
    (fun acc (i, j) ->
      Char.(cup acc (range (Uchar.of_char i) (Uchar.of_char j))))
    Char.empty

let ( !. ) =
  List.fold_left
    (fun acc i -> Char.cup acc (Char.singleton (Uchar.of_char i)))
    Char.empty

let l1 = !![ 65, 100; 255, 300; 450, 900 ]
let l2 = !![ 70, 99; 100, 255; 350, 400; 402, 800 ]
let l1_cup_l2 = !![ 65, 300; 350, 400; 402, 900 ]
let l1_cap_l2 = !![ 70, 99; 100, 100; 255, 255; 450, 800 ]
let l1_diff_l2 = !![ 65, 69; 256, 300; 801, 900 ]
let l2_diff_l1 = !![ 101, 254; 350, 400; 402, 449 ]

let () =
  let check = check (module Char) in
  let open Char in
  run "Char"
    [
      ( "union",
        [
          check !@[ 'A', 'Z' ] (cup !@[ 'A', 'B' ] !@[ 'C', 'Z' ]);
          check !@[ 'A', 'Z' ] (cup !@[ 'A', 'Z' ] !@[ 'C', 'Z' ]);
          check !@[ 'A', 'E' ] (cup !.[ 'A'; 'B'; 'D' ] !.[ 'C'; 'E' ]);
          check any !![ 0, 10; 11, 100; 101, 200; (201, Uchar.(to_int max)) ];
          check l1_cup_l2 (cup l1 l2);
        ] );
      ( "inter",
        [
          check empty (cap !@[ 'A', 'B' ] !@[ 'C', 'Z' ]);
          check !@[ 'D', 'G' ] (cap !@[ 'A', 'G' ] !@[ 'D', 'X' ]);
          check !.[ 'A'; 'E' ] (cap !.[ 'A'; 'E'; 'D' ] !.[ 'E'; 'A'; 'C' ]);
          check any (cap any any);
          check l1_cap_l2 (cap l1 l2);
        ] );
      ( "diff",
        [
          check empty (diff empty any);
          check any (diff any empty);
          check empty (diff any any);
          check !![ 0, 64; (91, Uchar.(to_int max)) ] (diff any !@[ 'A', 'Z' ]);
          check l1_diff_l2 (diff l1 l2);
          check l2_diff_l1 (diff l2 l1);
        ] );
    ]
