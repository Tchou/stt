open Utils
open Stt

let a = Var.make "a"
let b = Var.make "b"

let bump n v =
  let set = ref (Var.Set.singleton v) in
  for _ = 0 to n - 1 do
    let v = Var.make (Var.name_not_in !set) in
    set := Var.Set.add v !set
  done;
  Var.name_not_in !set

let () =
  let checks = check T.string in
  run "Base.Bdd"
    [
      ( "names",
        [
          checks "a" a.Var.name; checks "b" b.Var.name; checks "fu" (bump 150 a);
        ] );
    ]
