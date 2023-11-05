type ('a, 't) bdd =
  | False
  | True of { id : int; leaf : 't }
  | Node of { id : int; var : 'a; low : ('a, 't) bdd; hi : ('a, 't) bdd }

(* Only added in OCaml 4.13-4.14 *)

let memoize f arg =
  let r = ref (fun () -> assert false) in
  (r :=
     fun () ->
       let res = f arg in
       (r := fun () -> res);
       res);
  fun () -> !r ()

let rec append_seq s1 (s2 : 'a Seq.t) () =
  match s1 () with
    Seq.Nil -> s2 ()
  | Seq.Cons(e, ss1) ->
    Seq.Cons(e, memoize (append_seq ss1 s2)())

let rec map_seq f s () =
  match s () with
    Seq.Nil -> Seq.Nil
  | Seq.Cons (e, ss) ->
    Seq.Cons (f e, memoize (map_seq f ss)())

let rec flat_map_seq f s () =
  match s () with
    Seq.Nil -> Seq.Nil
  | Seq.Cons(e, ss) ->
    (memoize (append_seq (f e) (memoize (flat_map_seq f ss)())) ()) ()


let ignore_first x _ = x
let ignore_false _ = False
let ignore_false2 _ _ = False

module Make (X : Common.T) (L : Sigs.Set) = struct
  type atom = X.t
  type leaf = L.t
  type t = (atom, leaf) bdd

  let name = Printf.sprintf "Bdd (%s) (%s)" X.name L.name
  let equal t1 t2 = t1 == t2
  let hash = function False -> 0 | True { id; _ } -> id | Node { id; _ } -> id
  let compare t1 t2 = compare (hash t1) (hash t2)

  let rec pp fmt t =
    let open Format in
    match t with
    | False -> fprintf fmt "@[False@]"
    | True { id; leaf } -> fprintf fmt "@[True (%d,@[%a@])@]" id L.pp leaf
    | Node { id; var; low; hi } ->
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
        | True r1, True r2 -> L.equal r1.leaf r2.leaf
        | Node n1, Node n2 ->
          X.equal n1.var n2.var && equal n1.low n2.low && equal n1.hi n2.hi
        | _ -> false

      let hash = function
        | False -> 0
        | True { leaf; _ } -> L.hash leaf
        | Node { var; low; hi; _ } ->
          let hl = hash low in
          let hh = hash hi in
          X.hash var + ((hl lsl 4) + hl) + ((hh lsl 16) + hl)
    end)

  let memo_node = HNode.create 16
  let uid = ref 1

  let true_ leaf =
    if L.is_empty leaf then False
    else
      let n = True { id = !uid; leaf } in
      try HNode.find memo_node n with
      | Not_found ->
        HNode.add memo_node n n;
        incr uid;
        n

  let node var ~low ~hi =
    if low == hi then low
    else
      let n = Node { id = !uid; var; low; hi } in
      try HNode.find memo_node n with
      | Not_found ->
        HNode.add memo_node n n;
        incr uid;
        n

  let empty = False
  let is_empty = function False -> true | _ -> false
  let any = true_ L.any
  let is_any = function True { leaf; _ } -> L.is_any leaf | _ -> false
  let atom x = node x ~low:empty ~hi:any

  let single_atom = function
      Node {var; low=False; hi; _} when is_any hi -> Some (var,true)
    | Node {var; low; hi=False; _} when is_any low -> Some (var, false)
    | _ -> None

  let leaf l = true_ l

  let rec neg = function
    | False -> any
    | True { leaf; _ } -> true_ (L.neg leaf)
    | Node { var; low; hi; _ } -> node var ~low:(neg low) ~hi:(neg hi)

  type operation = {
    eq : t -> t -> t;
    f_o : t -> t;
    o_f : t -> t;
    t_t : leaf -> leaf -> leaf;
  }

  let rec apply op t1 t2 =
    if t1 == t2 then op.eq t1 t2
    else
      match t1, t2 with
      | False, _ -> op.f_o t2
      | _, False -> op.o_f t1
      | True r1, True r2 -> true_ (op.t_t r1.leaf r2.leaf)
      | Node { var; low; hi; _ }, (True _ as t) ->
        let low = apply op low t in
        let hi = apply op hi t in
        node var ~low ~hi
      |(True _ as t), Node { var; low; hi; _ } ->
        let low = apply op t low in
        let hi = apply op t hi in
        node var ~low ~hi
      | Node r1, Node r2 ->
        let c = X.compare r1.var r2.var in
        let var, l1, l2, h1, h2 =
          if c = 0 then r1.var, r1.low, r2.low, r1.hi, r2.hi
          else if c > 0 then r2.var, t1, r2.low, t1, r2.hi
          else r1.var, r1.low, t2, r1.hi, t2
        in
        let low = apply op l1 l2 in
        let hi = apply op h1 h2 in
        node var ~low ~hi

  let cup =
    apply { eq = ignore_first; f_o = Fun.id; o_f = Fun.id; t_t = L.cup }

  let cap =
    apply { eq = ignore_first; f_o = ignore_false; o_f = ignore_false; t_t = L.cap }

  let diff =
    apply { eq = ignore_false2; f_o = ignore_false; o_f = Fun.id; t_t = L.diff }

  open Common
  module Conj = Pair (Pair (List (X)) (List (X))) (L)
  module Disj = List (Conj)

  let leaf_ t = leaf t
  let rec map ~(atom:(atom -> (atom, leaf) bdd))
      ~(leaf:leaf -> leaf) t =
    match t with
      False -> False
    | True r -> leaf_ (leaf r.leaf)
    | Node {var;low;hi;_} ->
      let res = atom var in
      let hi_res = map ~atom ~leaf hi in
      let low_res = map ~atom ~leaf low in
      cup (cap hi_res res) (diff low_res res)

  let fold ~atom ~leaf ~cup ~empty ~any t =
    let rec loop acc_cup acc_cap = function
        False -> empty
      | True {leaf=l;_} ->
        cup acc_cup (leaf acc_cap l)
      | Node { var; low; hi; _ } ->
        if is_empty low then loop acc_cup (atom true acc_cap var) hi
        else if is_empty hi then loop acc_cup (atom false acc_cap var) low
        else
          let acc_cup = loop acc_cup (atom true acc_cap var) hi in
          loop acc_cup (atom false acc_cap var) low
    in
    loop empty any t


  let dnf t : Conj.t Seq.t =
    let empty () = Seq.Nil in
    let any = [], [] in
    let atom b (p, n) v = if b then v::p, n else p, v::n in
    let cup disj line () = Seq.Cons (line, memoize disj ()) in
    let leaf line l = line, l in
    memoize (fold ~atom ~cup ~leaf ~empty ~any) t ()


  type elem = Conj.t
  let rcomp a b = X.compare b a
  let mem ((pos, neg), l) t =
    let open Stdlib in
    let e = (List.sort rcomp pos, List.sort rcomp neg), l in
    let rec loop d =
      match d () with
        Seq.Nil -> false
      | Seq.Cons (l, ds) -> (Conj.equal e l) || loop ds
    in loop (dnf t)

  let sample t =
    match (dnf t) () with
      Seq.Nil -> None
    | Seq.Cons (l, _) -> Some l

  let intersect t1 t2 = not (is_empty (cap t1 t2))
  let singleton ((pos, neg), l) =
    let open Stdlib in
    let acc = leaf l in
    let acc = List.fold_left (fun acc a -> cap acc (atom a)) acc pos in
    List.fold_left (fun acc a -> diff acc (atom a)) acc neg

end

module MakeLevel2 (X : Common.T) (L : Sigs.Bdd) = struct
  module Leaf = L
  include Make (X) (Leaf)
  let name = Printf.sprintf "Bdd2 (%s) (%s)" X.name L.name

  let expand_dnf (x_atoms, leaf) =
    let l_dnf = L.dnf leaf in
    map_seq (fun (l_atoms, _) -> (x_atoms, l_atoms)) l_dnf

  let full_dnf (t : t) =
    flat_map_seq expand_dnf (dnf t)

end
