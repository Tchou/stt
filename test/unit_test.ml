open Utils
open Stt

let () =
  let checkb = check T.bool in
  let check = check (module Unit) in
  let open Unit in
  run "Unit"
    [
      "basic", [ checkb true (is_any (singleton ())) ];
      "union", [ check empty (cup empty empty); check any (cup any empty) ];
      "inter", [ check any (cap any any); check empty (cap any empty) ];
    ]
