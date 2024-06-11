type t = bool
type elem = unit
let compare x y = if x == y then 0 else if y then 1 else -1
let equal x y = x == y
let hash = function false -> 0 | true -> 1
let name = "Unit"
let pp fmt x = Format.pp_print_string fmt (if x then name else "Empty")
let export _ = assert false
let singleton () = true
let cup x y = x || y
let cap x y = x && y
let neg x = not x
let diff x y = x && not y
let empty = false
let any = true
let is_empty x = not x
let is_any x = x
let intersect x y = x && y
let opt = Some ()
let sample = function false -> None | true -> opt
let mem () x = x
