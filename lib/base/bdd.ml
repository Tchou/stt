type ('a, 't) bdd =
  | False
  | True of int * 't
  | Node of (int * 'a * ('a, 't) bdd * ('a, 't) bdd)

module Make (X : Common.T) (L : Sigs.PreSet) = struct
  type atom = X.t
  type leaf = L.t
  type t = (atom, leaf) bdd

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
        fprintf fmt "@[Node (@[%d,@ @[%a@],@ @[%a@],@ @[%a@]@])@]" id X.pp var
          pp low pp hi

  module HNode = Hashtbl.Make (struct
    type nonrec t = t

    let rec equal : t -> t -> bool =
     fun b1 b2 ->
      b1 == b2
      ||
      match b1, b2 with
      | False, False -> true
      | True (_, l1), True (_, l2) -> L.equal l1 l2
      | Node (_, var1, low1, hi1), Node (_, var2, low2, hi2) ->
          X.equal var1 var2 && equal low1 low2 && equal hi1 hi2
      | _ -> false

    let hash = function
      | False -> 0
      | True (_, l) -> L.hash l
      | Node (_, var, low, hi) ->
          let hl = hash low in
          let hh = hash hi in
          X.hash var + ((hl lsl 4) + hl) + ((hh lsl 16) + hl)
  end)

  let memo_node = HNode.create 16
  let uid = ref 1

  let true_ t =
    if L.is_empty t then False
    else
      let n = True (!uid, t) in
      try HNode.find memo_node n with
      | Not_found ->
          HNode.add memo_node n n;
          incr uid;
          n

  let node var ~low ~hi =
    if low == hi then low
    else
      let n = Node (!uid, var, low, hi) in
      try HNode.find memo_node n with
      | Not_found ->
          HNode.add memo_node n n;
          incr uid;
          n

  let empty = False
  let is_empty = function False -> true | _ -> false
  let any = true_ L.any
  let is_any = function True (_, l) -> L.is_any l | _ -> false
  let atom x = node x ~low:empty ~hi:any
  let leaf l = true_ l

  let rec neg = function
    | False -> any
    | True (_, t) -> true_ (L.neg t)
    | Node (_, var, low, hi) -> node var ~low:(neg low) ~hi:(neg hi)

  let rec apply base_eq base_f_o base_o_f base_t_t t1 t2 =
    if t1 == t2 then base_eq t1 t2
    else
      match t1, t2 with
      | False, _ -> base_f_o t2
      | _, False -> base_o_f t1
      | True (_, l1), True (_, l2) -> true_ (base_t_t l1 l2)
      | Node (_, var, low, hi), (True _ as t)
       |(True _ as t), Node (_, var, low, hi) ->
          node var
            ~low:(apply base_eq base_f_o base_o_f base_t_t low t)
            ~hi:(apply base_eq base_f_o base_o_f base_t_t hi t)
      | Node (_, var1, low1, hi1), Node (_, var2, low2, hi2) ->
          let c = X.compare var1 var2 in
          if c = 0 then
            node var1
              ~low:(apply base_eq base_f_o base_o_f base_t_t low1 low2)
              ~hi:(apply base_eq base_f_o base_o_f base_t_t hi1 hi2)
          else if c < 0 then
            node var1
              ~low:(apply base_eq base_f_o base_o_f base_t_t low1 t2)
              ~hi:(apply base_eq base_f_o base_o_f base_t_t hi1 t2)
          else
            node var2
              ~low:(apply base_eq base_f_o base_o_f base_t_t t1 low2)
              ~hi:(apply base_eq base_f_o base_o_f base_t_t t1 hi2)

  let ignore_false _ = False
  let ignore_false2 _ _ = False
  let cup = apply (fun t1 _ -> t1) Fun.id Fun.id L.cup
  let cap = apply (fun t1 _ -> t1) ignore_false ignore_false L.cap
  let diff = apply ignore_false2 ignore_false Fun.id L.diff
end
