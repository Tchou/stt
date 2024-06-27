open Base

let pp_enum fmt (s: Hstring.t) = Format.fprintf fmt "`%s" Hstring.(!!s)
module H = struct
  include Hstring

  let pp = pp_enum
end

include FiniteCofinite.Make (H)

let name = "Enum"

(*
    TODO : print "Bool" instead of "`true | `false"

let export s =
  let bool = S.of_list [
    (Hstring.cons "true");
    (Hstring.cons "false")
  ]
  in
  let c, s' =
    match s with
    | `Finite s -> false, s
    | `Cofinite s -> true, s
  in
  if S.equal s' bool then
    c, [ s, Pr_basic.Singleton (fun fmt -> Format.fprintf fmt "Bool") ]
  else
    export s *)

let pp fmt s =
  Pr_basic.(pp ~pp_any:(pr_string name) fmt @@ export s)

let sample = function
  | `Finite s -> S.min_elt_opt s
  | `Cofinite s ->
    let rec loop base i suff =
      let atom = Hstring.cons (Hstring.(!!base) ^ suff) in
      if S.mem atom s then loop base (i + 1) (string_of_int i) else atom
    in
    Some (loop (Hstring.cons "cst") 0 "")
