open Base

include FiniteCofinite.Make (Hstring)
let pp_enum fmt (s: Hstring.t) = Format.fprintf fmt "`%s" Hstring.(!!s)

let name = "Enum"

let pp fmt (s:t) =
  let open Format in
  let c, s = match s with `Finite s -> false, s | `Cofinite s -> true, s in
  let l = S.elements s in
  fprintf fmt "@[";
  if c then fprintf fmt "%s" name;
  let () =
    match l with
    | [] -> if not c then fprintf fmt "Empty"
    | _ :: ll ->
      if c then fprintf fmt "\\";
      if c && ll <> [] then fprintf fmt "(";
      pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt "|") pp_enum fmt l;
      if c && ll <> [] then fprintf fmt ")"
  in
  fprintf fmt "@]"

let sample = function
  | `Finite s -> S.min_elt_opt s
  | `Cofinite s ->
    let rec loop base i =
      let atom = Hstring.cons (Hstring.(!!base) ^ string_of_int i) in
      if S.mem atom s then loop base (i + 1) else atom
    in
    Some (loop (Hstring.cons "atom") 0)
