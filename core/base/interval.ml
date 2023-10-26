open Common

module type V = sig
  include T

  val min : t
  val max : t
  val succ : t -> t
  val pred : t -> t
end

module Make (X : V) = struct
  include List (Pair (X) (X))

  type elem = X.t

  let name = "Interval"
  let empty = []
  let is_empty = function [] -> true | _ -> false
  let any_range = X.min, X.max
  let any = [ any_range ]
  let is_any x = equal any x
  let ( < ) x1 x2 = X.compare x1 x2 < 0
  let ( = ) x1 x2 = X.equal x1 x2
  let ( > ) x1 x2 = X.compare x1 x2 > 0
  let ( <= ) x1 x2 = X.compare x1 x2 <= 0
  let ( >= ) x1 x2 = X.compare x1 x2 >= 0
  let min x1 x2 = if x2 < x1 then x2 else x1
  let max x1 x2 = if x2 > x1 then x2 else x1

  let rec ( @:: ) (a, b) l =
    match l with
    | [] -> [ a, b ]
    | (c, d) :: ll ->
        (* invariant, b < c *)
        if b = X.max then [ a, b ]
        else if X.succ b = c then (a, d) @:: ll
        else (a, b) :: l

  let rec cup i1 i2 =
    match i1, i2 with
    | [], l | l, [] -> l
    | ((a1, b1) as c1) :: ii1, ((a2, b2) as c2) :: ii2 ->
        if a1 > b2 then c2 @:: cup i1 ii2
        else if a2 > b1 then c1 @:: cup ii1 i2
        else
          let u = min a1 a2 in
          if b1 < b2 then cup ii1 ((u, b2) @:: ii2)
          else if b1 = b2 then (u, b1) @:: cup ii1 ii2
          else cup ((u, b1) @:: ii1) ii2

  let rec cap i1 i2 =
    match i1, i2 with
    | [], _ | _, [] -> []
    | (a1, b1) :: ii1, (a2, b2) :: ii2 ->
        if a1 > b2 then cap i1 ii2
        else if a2 > b1 then cap ii1 i2
        else
          let u = max a1 a2 in
          if b1 < b2 then (u, b1) @:: cap ii1 ((X.succ b1, b2) @:: ii2)
          else if b1 = b2 then (u, b1) @:: cap ii1 ii2
          else (u, b2) @:: cap ((X.succ b2, b1) @:: ii1) ii2

  let rec diff i1 i2 =
    match i1, i2 with
    | ([] as l), _ | l, [] -> l
    | ((a1, b1) as c1) :: ii1, (a2, b2) :: ii2 ->
        if b1 < a2 then c1 @:: diff ii1 i2
        else if b2 < a1 then diff i1 ii2
        else if a2 <= a1 then
          if b2 < b1 then diff ((X.succ b2, b1) @:: ii1) ii2 else diff ii1 i2
        else if (* a1 < a2 *)
                b2 >= b1 then diff ((a1, X.pred a2) @:: ii1) i2
        else diff ((a1, X.pred a2) @:: (X.succ b2, b1) @:: ii1) ii2

  (*
   -inf          +inf :: []
         (0,0)   :: []
*)

  let neg i = diff any i
  let range a b = [ a, b ]
  let singleton a = range a a

  let rec intersect i1 i2 =
    match i1, i2 with
    | [], _ | _, [] -> false
    | (a1, b1) :: ii1, (a2, b2) :: ii2 ->
        if a1 > b2 then intersect i1 ii2
        else if a2 > b1 then intersect ii1 i2
        else true

  let sample = function [] -> None | (a, _) :: _ -> Some a

  let rec mem x i =
    match i with
    | [] -> false
    | (a, b) :: ii -> if x < a then false else if x > b then mem x ii else true
end
