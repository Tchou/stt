open Utils

let pp_atom fmt s = Format.fprintf fmt "`%s" s

include FiniteCofinite.Make (struct
  include String

  let pp = pp_atom
  let hash s = Hashtbl.hash s
end)

let name = "Atom"

let pp fmt s =
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
        pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt "|") pp_atom fmt l;
        if c && ll <> [] then fprintf fmt ")"
  in
  fprintf fmt "@]"

let sample = function
  | `Finite s -> S.min_elt_opt s
  | `Cofinite s ->
      let rec loop name i =
        let atom = name ^ string_of_int i in
        if S.mem atom s then loop name (i + 1) else atom
      in
      Some (loop "atom" 0)
