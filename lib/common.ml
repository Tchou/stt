module type T = sig
  type t

  val compare : t -> t -> int
  val equal : t -> t -> bool
  val hash : t -> int
  val pp : Format.formatter -> t -> unit
end

module Pair (X : T) (Y : T) : T with type t = X.t * Y.t = struct
  type t = X.t * Y.t

  let compare p1 p2 =
    if p1 == p2 then 0
    else
      let x1, y1 = p1 in
      let x2, y2 = p2 in
      let c = X.compare x1 x2 in
      if c <> 0 then c else Y.compare y1 y2

  let equal p1 p2 =
    p1 == p2
    ||
    let x1, y1 = p1 in
    let x2, y2 = p2 in
    X.equal x1 x2 && Y.equal y1 y2

  let hash (x, y) =
    let x = X.hash x in
    let y = Y.hash y in
    ((x lsl 13) - x + y) land max_int

  let pp fmt (x, y) = Format.fprintf fmt "(%a, %a)" X.pp x Y.pp y
end

module List (X : T) : T with type t = X.t list = struct
  type t = X.t list

  let rec compare l1 l2 =
    if l1 == l2 then 0
    else
      match l1, l2 with
      | [], [] -> 0
      | [], _ -> -1
      | _, [] -> 1
      | x1 :: ll1, x2 :: ll2 ->
          let c = X.compare x1 x2 in
          if c <> 0 then c else compare ll1 ll2

  let equal l1 l2 = 0 = compare l1 l2

  let hash l =
    let rec loop l acc =
      match l with
      | [] -> acc
      | x :: ll ->
          let x = X.hash x in
          loop ll (acc + ((x lsl 7) - x))
    in
    loop l 0

  let pp fmt l =
    let open Format in
    fprintf fmt "%a"
      (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt "; ") X.pp)
      l
end

module Uchar = struct
  include Uchar
  let pp fmt u =
    let i = Uchar.to_int u in
    if
      (i >= 0x00 && i < 0x1f)
      || i == 0x7f
      || (i >= 0x80 && i <= 0x9f)
      || i >= 0x40000
    then Format.fprintf fmt "\\u%x" i
    else
      let b = Bytes.create (Uchar.utf_8_byte_length u) in
      ignore (Bytes.set_utf_8_uchar b 0 u);
      Format.pp_print_bytes fmt b
end
