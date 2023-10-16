type ('a, 't) bdd =
  | False
  | True of int * 't
  | Node of (int * 'a * ('a, 't) bdd * ('a, 't) bdd)

module Make (X : Common.T) (L : Sigs.PreSet) = struct
  type t = (X.t, L.t) bdd
  type atom = X.t
  type leaf = L.t

  let equal t1 t2 = t1 == t2

  let hash = function
    | False -> 0
    | True (id, _) -> id
    | Node (id, _, _, _) -> id

  let compare t1 t2 = compare (hash t1) (hash t2)

  let rec pp fmt t =
    let open Format in
    match t with
    | False -> fprintf fmt "@[False@]"
    | True (id, l) -> fprintf fmt "@[True (%d,@[%a@])@]" id L.pp l
    | Node (id, var, low, hi) ->
        fprintf fmt "@[Node (@[%d,@ @[%a@],@ @[%a@],@ @[%a@]@])@]" id X.pp var pp
          low pp hi

  module H = Hashtbl.Make (struct
    type nonrec t = t

    let rec equal b1 b2 =
      b1 == b2
      ||
      match b1, b2 with
      | Node (_, var1, low1, hi1), Node (_, var2, low2, hi2) ->
          X.equal var1 var2 && equal low1 low2 && equal hi1 hi2
      | True (_, t1), True (_, t2) -> L.equal t1 t2
      | _ -> false

    let hash = function
      | False -> 0
      | True (_, t) -> L.hash t
      | Node (_, var, low, hi) ->
          let hl = hash low in
          let hh = hash hi in
          X.hash var + ((hl lsl 4) + hl) + ((hh lsl 16) + hl)
  end)

  let memo = H.create 16
  let uid = ref 1

  let true_ t =
    if L.is_empty t then False
    else
      let n = True (!uid, t) in
      try H.find memo n with
      | Not_found ->
          H.add memo n n;
          incr uid;
          n

  let node var ~low ~hi =
    if low == hi then low
    else
      let n = Node (!uid, var, low, hi) in
      try H.find memo n with
      | Not_found ->
          H.add memo n n;
          incr uid;
          n

  let empty = False
  let is_empty t = t == False
  let any = true_ L.any
  let is_any l = l == any
  let atom x = node x ~low:empty ~hi:any
  let leaf l = true_ l

  let rec neg = function
    | False -> any
    | True (_, t) -> true_ (L.neg t)
    | Node (_, var, low, hi) -> node var ~low:(neg low) ~hi:(neg hi)

  let rec apply base_op t1 t2 =
    base_op
      (fun t1 t2 ->
        match t1, t2 with
        | Node (_, var, low, hi), (True _ as t)
         |(True _ as t), Node (_, var, low, hi) ->
            node var ~low:(apply base_op low t) ~hi:(apply base_op hi t)
        | Node (_, var1, low1, hi1), Node (_, var2, low2, hi2) ->
            let c = X.compare var1 var2 in
            if c = 0 then
              node var1 ~low:(apply base_op low1 low2)
                ~hi:(apply base_op hi1 hi2)
            else if c < 0 then
              node var1 ~low:(apply base_op low1 t2) ~hi:(apply base_op hi1 t2)
            else
              node var2 ~low:(apply base_op t1 low2) ~hi:(apply base_op t1 hi2)
        | _ -> assert false)
      t1 t2

  let base_cup k t1 t2 =
    if t1 == t2 then t1
    else
      match t1, t2 with
      | False, t | t, False -> t
      | True (_, l1), True (_, l2) -> true_ (L.cup l1 l2)
      | _ -> k t1 t2

  let base_cap k t1 t2 =
    if t1 == t2 then t1
    else
      match t1, t2 with
      | False, _ | _, False -> False
      | True (_, l1), True (_, l2) -> true_ (L.cap l1 l2)
      | _ -> k t1 t2

  let base_diff k t1 t2 =
    if t1 == t2 then False
    else
      match t1, t2 with
      | (False as t), _ | t, False -> t
      | True (_, l1), True (_, l2) -> true_ (L.diff l1 l2)
      | _ -> k t1 t2

  let cup = apply base_cup
  let cap = apply base_cap
  let diff = apply base_diff
end
