open Base

let pp_char fmt u =
  let open Uchar in
  let i = to_int u in
  if
    (i >= 0x00 && i < 0x1f)
    || i == 0x7f
    || (i >= 0x80 && i <= 0x9f)
    || i >= 0x40000
  then Format.fprintf fmt "\\u%x" i
  else
    let b = Bytes.create (utf_8_byte_length u) in
    ignore (Bytes.set_utf_8_uchar b 0 u);
    Format.pp_print_bytes fmt b

include Interval.Make (struct
  include Uchar
  let name = "Uchar"
  let pp = pp_char
end)

let name = "Char"

let pp_pair fmt (a, b) =
  let open Format in
  fprintf fmt "'%a'" pp_char a;
  if a != b then fprintf fmt "--'%a'" pp_char b

let pp fmt l =
  let open Format in
  if is_any l then pp_print_string fmt name
  else if is_empty l then pp_print_string fmt "Empty"
  else
    pp_print_list
      ~pp_sep:(fun fmt () -> fprintf fmt "|")
      pp_pair fmt
      (l :> (elem * elem) list)
