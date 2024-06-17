open Base

module X = struct
  type t =
    | NegInf
    | PosInf
    | Num of Z.t

  let name = "ℤ"
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

include Interval.Make (X)

let name = "Int"

type elem = Z.t

let pp fmt l =
  Base.Pr_basic.(pp ~pp_any:(pr_string name) fmt @@ export l)

let mem x i = mem (Num x) i
let left a = range X.NegInf (X.Num a)
let right a = range (X.Num a) X.PosInf
let range a b = range (X.Num a) (X.Num b)
let singleton a = range a a

let sample i =
  match i with
  | [] -> None
  | (X.NegInf, X.PosInf) :: _ -> Some Z.zero
  | (X.Num a, _) :: _ -> Some a
  | (_, X.Num b) :: _ -> Some b
  | _ -> assert false