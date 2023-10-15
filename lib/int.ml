open Base

module X = struct
  type t =
    | NegInf
    | PosInf
    | Num of Z.t

  let equal a b =
    a == b
    ||
    match a, b with
    | NegInf, NegInf | PosInf, PosInf -> true
    | Num x, Num y -> Z.equal x y
    | _ -> false

  let compare a b =
    match a, b with
    | NegInf, NegInf | PosInf, PosInf -> 0
    | NegInf, _ | _, PosInf -> -1
    | PosInf, _ | _, NegInf -> 1
    | Num x, Num y -> Z.compare x y

  let hash = function NegInf -> 0 | PosInf -> 1 | Num x -> 2 + Z.hash x

  let pp fmt a =
    let s =
      match a with NegInf -> "-*" | PosInf -> "*" | Num x -> Z.to_string x
    in
    Format.pp_print_string fmt s

  let min = NegInf
  let max = PosInf
  let succ = function Num x -> Num (Z.succ x) | _ -> failwith "succ"
  let pred = function Num x -> Num (Z.pred x) | _ -> failwith "pred"
end

module I = Interval.Make (X)

let name = "Int"

type t = I.t
type elem = Z.t

let compare = I.compare
let equal = I.equal
let hash = I.hash

let pp_pair fmt (a, b) =
  let open Format in
  fprintf fmt "%a" X.pp a;
  if not (X.equal a b) then fprintf fmt "--%a" X.pp b

let pp fmt l =
  let open Format in
  if I.is_any l then pp_print_string fmt name
  else if I.is_empty l then pp_print_string fmt "Empty"
  else pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt "|") pp_pair fmt l

let empty = I.empty
let is_empty = I.is_empty
let any = I.any
let is_any = I.is_any
let cup = I.cup
let cap = I.cap
let diff = I.diff
let neg = I.neg
let intersect = I.intersect
let mem x i = I.mem (Num x) i
let left a = I.range X.NegInf (X.Num a)
let right a = I.range (X.Num a) X.PosInf
let range a b = I.range (X.Num a) (X.Num b)
let singleton a = range a a

let sample i =
  match i with
  | [] -> None
  | (X.NegInf, X.PosInf) :: _ -> Some Z.zero
  | (X.Num a, _) :: _ -> Some a
  | (_, X.Num b) :: _ -> Some b
  | _ -> assert false
