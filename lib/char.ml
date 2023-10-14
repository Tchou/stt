type atom = Common.Uchar.t

let name = "Char"

include Interval.Make (Common.Uchar)

let pp_pair fmt (a, b) =
  let open Format in
  fprintf fmt "'%a'" Common.Uchar.pp a;
  if a != b then fprintf fmt "--'%a'" Common.Uchar.pp b

let pp fmt l =
  let open Format in
  if is_any l then pp_print_string fmt name
  else if is_empty l then pp_print_string fmt "Empty"
  else
    pp_print_list
      ~pp_sep:(fun fmt () -> fprintf fmt "|")
      pp_pair fmt
      (l :> (atom * atom) list)

let atom a = range a a
