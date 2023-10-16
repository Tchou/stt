open Test_utils

module B =
  Base.Bdd.Make
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

let () =
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
        ] );
    ]
