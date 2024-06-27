module Make (X : Common.T) : sig
  module S : Set.S with type elt = X.t

  include
    Sigs.FiniteCofinite
    with type elem = X.t
     and type t =
           [ `Finite of S.t
           | `Cofinite of S.t
           ]
end = struct
  module S = Set.Make (X)

  type t =
    [ `Finite of S.t
    | `Cofinite of S.t
    ]

  type elem = X.t

  let compare s1 s2 =
    match s1, s2 with
    | `Finite s1, `Finite s2 | `Cofinite s1, `Cofinite s2 -> S.compare s1 s2
    | `Finite _, `Cofinite _ -> 1
    | `Cofinite _, `Finite _ -> -1

  let equal s1 s2 =
    s1 == s2
    ||
    match s1, s2 with
    | `Finite s1, `Finite s2 | `Cofinite s1, `Cofinite s2 -> S.equal s1 s2
    | _ -> false

  let hash s =
    let c, s = match s with `Finite s -> false, s | `Cofinite s -> true, s in
    let h =
      S.fold
        (fun e acc ->
           let h = X.hash e in
           (h lsl 4) + h + acc)
        s 0
    in
    (if c then (h lsl 13) - h else h) land max_int

  let empty = `Finite S.empty
  let is_empty = function `Finite s -> S.is_empty s | _ -> false
  let any = `Cofinite S.empty
  let is_any = function `Cofinite s -> S.is_empty s | _ -> false
  let name = "FiniteCofinite"
  let singleton x = `Finite (S.singleton x)
  let neg = function `Finite s -> `Cofinite s | `Cofinite s -> `Finite s
  let is_finite = function `Finite _ -> true | _ -> false

  let mem x s =
    match s with `Finite s -> S.mem x s | `Cofinite s -> not (S.mem x s)

  let cup s1 s2 =
    match s1, s2 with
    | `Finite s1, `Finite s2 -> `Finite (S.union s1 s2)
    | `Finite f, `Cofinite c | `Cofinite c, `Finite f -> `Cofinite (S.diff c f)
    | `Cofinite s1, `Cofinite s2 -> `Cofinite (S.inter s1 s2)

  let cap s1 s2 =
    match s1, s2 with
    | `Finite s1, `Finite s2 -> `Finite (S.inter s1 s2)
    | `Finite f, `Cofinite c | `Cofinite c, `Finite f -> `Finite (S.diff f c)
    | `Cofinite s1, `Cofinite s2 -> `Cofinite (S.union s1 s2)

  let diff s1 s2 = cap s1 (neg s2)

  let intersect s1 s2 =
    match s1, s2 with
    | `Finite s1, `Finite s2 -> not (S.disjoint s1 s2)
    | `Finite f, `Cofinite c | `Cofinite c, `Finite f -> not (S.subset f c)
    | _ -> true

  let pp _ _ = assert false
  let sample _ = assert false

  let export t =
    if is_any t then
      true, []
    else if is_empty t then
      false, []
    else
      let f (s : S.t) : (t * Pr_basic.single) list = 
        S.fold (
          fun (x : X.t)
              (acc : (t * Pr_basic.single) list) : (t * Pr_basic.single) list ->
            let f =
              fun (fmt : Format.formatter) : unit ->
                Format.fprintf fmt "%a" X.pp x
            in
            (singleton x, Pr_basic.Singleton f) :: acc
        ) 
        s []
      in
      match t with
      | `Finite s -> false, f s
      | `Cofinite s -> true, f s

end
