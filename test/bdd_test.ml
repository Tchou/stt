open Test_utils

module B =
  Stt.Base.Bdd.Make
    (struct
      include String

      let hash = Hashtbl.hash
      let pp fmt s = Format.fprintf fmt "%S" s
    end)
    (Stt.Unit)

let a = B.atom "A"
let b = B.atom "B"
let c = B.atom "C"
let bc = B.cap b c
let abc = B.(cap a (cap b c))
let nabc = B.(cap (neg a) (cap b c))
let a_b = B.(cup a b)
let b_c = B.(cup b c)
let a_nc = B.(cup a (neg c))
let comb1 = B.(cap a_b (cap b_c a_nc))

let comb1_alt =
  let open B in
  List.fold_left cup empty
    [ cap a b; cap (neg b) (cap c a); cap b (cap (neg c) (neg a)) ]

let dnf1 = B.dnf comb1
let ldnf1 = List.of_seq dnf1
let ldnf1_alt = List.of_seq (B.dnf comb1_alt)

let () =
  let checkd = check (module B.Disj) in
  let check = check (module B) in
  let open B in
  run "Base.Bdd"
    [
      ( "union",
        [
          check empty (cup empty empty);
          check any (cup any empty);
          check abc (cup abc abc);
          check bc (cup abc nabc);
        ] );
      ( "inter",
        [
          check any (cap any any);
          check empty (cap any empty);
          check abc (cap bc abc);
          check comb1_alt comb1;
        ] );
      "dnf", [ checkd ldnf1_alt ldnf1 ];
    ]
