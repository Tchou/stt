module type T = sig
  type t
  val name : string
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val hash : t -> int
  val pp : Format.formatter -> t -> unit
end

module Pair (X : T) (Y : T) : T with type t = X.t * Y.t = struct
  type t = X.t * Y.t

  let name = "(" ^ X.name ^ ", " ^ Y.name ^ ")"
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

module List (X : T) : sig
  include T with type t = X.t list

  val pp_ :
    ?sep:(Format.formatter -> unit -> unit) ->
    ?op:(Format.formatter -> unit -> unit) ->
    ?cl:(Format.formatter -> unit -> unit) ->
    Format.formatter ->
    t ->
    unit
end = struct
  type t = X.t list

  let name = "List (" ^ X.name ^ ")"
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

  open Format

  let pp_
      ?(sep = fun fmt () -> fprintf fmt "; ")
      ?(op = fun fmt () -> fprintf fmt "[")
      ?(cl = fun fmt () -> fprintf fmt "]")
      fmt
      l =
    fprintf fmt "%a%a%a" op () (pp_print_list ~pp_sep:sep X.pp) l cl ()

  let pp fmt l = pp_ fmt l
end

module String =
struct
  let name = "String"
  include Stdlib.String
  let hash s = Hashtbl.hash s
  let pp = Format.pp_print_string
end

module Bool =
struct
  let name = "Bool"
  include Stdlib.Bool
  let hash = function false -> 0 | true -> 1
  let pp = Format.pp_print_bool
end

module Let =
struct
  let (let<> ) c f =
    if c <> 0 then c else
      f ()

  let (let&) o f =
    match o with
      None -> None
    | Some x -> f x

  let (let|) o f =
    match o with
      Some _ -> o
    | None -> f ()
end
