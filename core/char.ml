open Base

let pp_char fmt u =
  let open Uchar in
  let i = to_int u in
  if
    i < 0x1f
    || i == 0x7f
    || (i >= 0x80 && i <= 0x9f)
    || i >= 0x40000
  then Format.fprintf fmt "'\\u{%04x}'" i
  else
    let b = Buffer.create 4 in
    let () = Buffer.add_utf_8_uchar b u in
    Format.fprintf fmt "'%s'" (Buffer.contents b)

include Interval.Make (struct
    include Uchar
    let name = "Uchar"
    let pp = pp_char
  end)

let name = "Char"

let pp fmt l =
  Base.Pr_basic.(pp ~pp_any:(pr_string name) fmt @@ export l)
