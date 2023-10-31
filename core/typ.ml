open Base

type ('atom, 'int, 'char, 'unit, 'product, 'arrow) descr_ = {
  atom : 'atom;
  int : 'int;
  char : 'char;
  unit : 'unit;
  product : 'product;
  arrow : 'arrow;
}
module Get =
struct
  let atom t = t.atom
  let int t = t.int
  let char t = t.char
  let unit t = t.unit
  let product t = t.product
  let arrow t = t.arrow
end
module Set =
struct
  let atom atom t = { t with atom }
  let int int t = { t with int }
  let char char t = { t with char }
  let unit unit t = { t with unit }
  let product product t = { t with product }
  let arrow arrow t   = { t with arrow }
end

type 'a node = {
  mutable descr : 'a;
  id : int;
}
module VarAtom =
struct
  include Bdd.Make (Var) (Atom)
  let get = Get.atom
  let set = Set.atom
end

module VarInt = struct
  include Bdd.Make (Var) (Int)
  let get = Get.int
  let set = Set.int
end

module VarChar = struct
  include Bdd.Make (Var) (Char)
  let get = Get.char
  let set = Set.char
end
module VarUnit = struct
  include Bdd.Make (Var) (Unit)
  let get = Get.unit
  let set = Set.unit
end


module rec Descr :
  (Common.T
   with type t =
          ( VarAtom.t,
            VarInt.t,
            VarChar.t,
            VarUnit.t,
            VarProduct_.t,
            VarProduct_.t )
            descr_) = struct
  type t =
    ( VarAtom.t,
      VarInt.t,
      VarChar.t,
      VarUnit.t,
      VarProduct_.t,
      VarProduct_.t )
      descr_

  let equal t1 t2 =
    t1 == t2
    || VarAtom.equal t1.atom t2.atom
       && VarInt.equal t1.int t2.int
       && VarChar.equal t1.char t2.char
       && VarUnit.equal t1.unit t2.unit
       && VarProduct_.equal t1.product t2.product
       && VarProduct_.equal t1.arrow t2.arrow

  let (let<> ) c f =
    if c <> 0 then c else
      f ()

  let compare t1 t2 =
    let<> () = VarAtom.compare t1.atom t2.atom in
    let<> () = VarInt.compare t1.int t2.int in
    let<> () = VarChar.compare t1.char t2.char in
    let<> () = VarUnit.compare t1.unit t2.unit in
    let<> () = VarProduct_.compare t1.product t2.product in
    let<> () = VarProduct_.compare t1.arrow t2.arrow in
    0
  let h v x = v + ((x lsl 5) - x)

  let hash t =
    h (VarAtom.hash t.atom) 0
    |> h (VarInt.hash t.int)
    |> h (VarChar.hash t.char)
    |> h (VarUnit.hash t.unit)
    |> h (VarProduct_.hash t.product)
    |> h (VarProduct_.hash t.arrow)

  let pp _fmt _t = ()
end

and Node : (Common.T with type t = Descr.t node) = struct
  type t = Descr.t node

  let equal (n1 : t) (n2 : t) = n1 == n2
  let compare (n1 : t) (n2 : t) = Stdlib.Int.compare n1.id n2.id
  let hash n = n.id
  let pp fmt n = Format.fprintf fmt "@[NODE:%d@]" n.id
end

and Product : (Sigs.Bdd with type atom = Node.t * Node.t) =
  Bdd.Make (Common.Pair (Node) (Node)) (Unit)

and VarProduct_ : Sigs.Bdd2 with type atom = Var.t
                             and type Leaf.t = Product.t
                             and type Leaf.atom = Product.atom
                             and type Leaf.leaf = Product.leaf

  = Bdd.MakeLevel2 (Var) (Product)

module VarProduct =
struct
  include VarProduct_
  let get = Get.product
  let set = Set.product
end
module VarArrow =
struct
  include VarProduct_
  let get = Get.arrow
  let set = Set.arrow
end
include Descr
type descr = t

module type Basic = sig
  include Base.Sigs.Bdd with type atom = Var.t
  val get : descr -> t
  val set : t -> descr -> descr
end

module type Constr = sig
  include Base.Sigs.Bdd2 with type atom = Var.t
  val get : descr -> t
  val set : t -> descr -> descr
end

let empty = {
  atom = VarAtom.empty;
  int = VarInt.empty;
  char = VarChar.empty;
  unit = VarUnit.empty;
  product = VarProduct.empty;
  arrow = VarProduct.empty
}
let any = { atom = VarAtom.any;
            int = VarInt.any;
            char = VarChar.any;
            unit = VarUnit.any;
            product = VarProduct.any;
            arrow = VarProduct.any}

module Singleton =
struct
  let atom a = {empty with atom = VarAtom.leaf (Atom.singleton a) }
  let int z = { empty with int = VarInt.leaf (Int.singleton z) }
  let char c = { empty with char = VarChar.leaf (Char.singleton c)}
  let unit = { empty with unit = VarUnit.any }

end

let node_uid = ref ~-1
let node t = incr node_uid; { id = !node_uid; descr = t }
let descr n = n.descr
let make () = node empty

let def n t =
  assert (n.descr == empty);
  n.descr <- t

let var_product n1 n2 =
  VarProduct.(leaf (Product.atom (n1, n2)))
let product n1 n2 =
  { empty with product = var_product n1 n2}

let arrow n1 n2 =
  { empty with arrow = var_product n1 n2}

let var v = {
  atom = VarAtom.atom v;
  int =VarInt.atom v;
  char = VarChar.atom v;
  unit = VarUnit.atom v;
  product = VarProduct.atom v;
  arrow = VarArrow.atom v
}

let cup t1 t2 = {
  atom = VarAtom.cup t1.atom t2.atom;
  int = VarInt.cup t1.int t2.int;
  char = VarChar.cup t1.char t2.char;
  unit = VarUnit.cup t1.unit t2.unit;
  product = VarProduct.cup t1.product t2.product;
  arrow = VarProduct.cup t1.arrow t2.arrow
}

let cap t1 t2 = {
  atom = VarAtom.cap t1.atom t2.atom;
  int = VarInt.cap t1.int t2.int;
  char = VarChar.cap t1.char t2.char;
  unit = VarUnit.cap t1.unit t2.unit;
  product = VarProduct.cap t1.product t2.product;
  arrow = VarProduct.cap t1.arrow t2.arrow
}

let diff t1 t2 = {
  atom = VarAtom.diff t1.atom t2.atom;
  int = VarInt.diff t1.int t2.int;
  char = VarChar.diff t1.char t2.char;
  unit = VarUnit.diff t1.unit t2.unit;
  product = VarProduct.diff t1.product t2.product;
  arrow = VarProduct.diff t1.arrow t2.arrow
}

let neg t = {
  atom = VarAtom.neg t.atom;
  int = VarInt.neg t.int;
  char = VarChar.neg t.char;
  unit = VarUnit.neg t.unit;
  product = VarProduct.neg t.product;
  arrow = VarProduct.neg t.arrow
}


let fold ~var ~atom ~int ~char ~unit ~product ~arrow ~cup ~cap ~diff ~empty ~any t =
  let basic (type l) (module M : Basic with type leaf = l) leaf t =
    M.fold ~atom:var ~cup ~cap ~diff ~leaf ~empty ~any (M.get t)
  in
  let constr (type l la) (module M : Constr with type Leaf.t = l and type Leaf.atom = la) latom t =
    M.fold ~atom:var ~cup ~cap ~diff ~leaf:(
      fun l conj ->
        cap conj (M.Leaf.fold ~atom:latom ~cup ~cap ~diff ~leaf:(fun _ acc -> acc) ~empty ~any l)
    ) ~empty ~any (M.get t)
  in
  let acc = basic (module VarInt) int t in
  let acc = cup acc (basic (module VarChar) char t) in
  let acc = cup acc (basic (module VarAtom) atom t) in
  let acc = cup acc (basic (module VarUnit) unit t) in
  let acc = cup acc (constr (module VarProduct) product t) in
  let acc = cup acc (constr (module VarArrow) arrow t) in
  acc

let munit f = fun x () -> f x

let iter ~var ~atom ~int ~char ~unit ~product ~arrow t =
  fold ~var:(var) ~atom:(munit atom)
    ~int:(munit int)
    ~char:(munit char)
    ~unit:(munit unit)
    ~product:(product)
    ~arrow:(arrow)
    ~cup:(fun () () -> ())
    ~cap:(fun () () -> ())
    ~diff:(fun () () -> ())
    ~empty:()
    ~any:()
    t

let map ~var ~atom ~int ~char ~unit ~product ~arrow t =
  let basic (type l) (module M : Basic with type leaf = l) f t acc =
    M.set (M.map ~atom:(fun v -> M.get (var v)) ~leaf:f (M.get t)) acc
  in
  let constr (type l la) (module M : Constr with type Leaf.t = l and type Leaf.atom = la) f t acc =
    M.set (M.map ~atom:(fun v -> M.get (var v)) ~leaf:(fun l ->
        M.Leaf.map ~atom:(fun a -> M.Leaf.atom (f a)) ~leaf:Fun.id l
      ) (M.get t)) acc
  in
  empty
  |> basic (module VarAtom) atom t
  |> basic (module VarInt) int t
  |> basic (module VarChar) char t
  |> basic (module VarUnit) unit t
  |> constr (module VarProduct) product t
  |> constr (module VarArrow) arrow t