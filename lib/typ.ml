open Base

type ('atom, 'int, 'char, 'unit, 'product, 'arrow) descr = {
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


type 'a node = {
  mutable descr : 'a;
  id : int;
}
module type VarBdd = Sigs.Bdd with type atom = Var.t
module VarAtom = Bdd.Make (Var) (Atom)
module VarInt = Bdd.Make (Var) (Int)
module VarChar = Bdd.Make (Var) (Char)
module VarUnit = Bdd.Make (Var) (Unit)

module rec Descr :
  (Common.T
   with type t =
          ( VarAtom.t,
            VarInt.t,
            VarChar.t,
            VarUnit.t,
            VarProduct.t,
            VarProduct.t )
            descr) = struct
  type t =
    ( VarAtom.t,
      VarInt.t,
      VarChar.t,
      VarUnit.t,
      VarProduct.t,
      VarProduct.t )
      descr

  let equal t1 t2 =
    t1 == t2
    || VarAtom.equal t1.atom t2.atom
       && VarInt.equal t1.int t2.int
       && VarChar.equal t1.char t2.char
       && VarUnit.equal t1.unit t2.unit
       && VarProduct.equal t1.product t2.product
       && VarProduct.equal t1.arrow t2.arrow

  let (let<> ) c f =
    if c <> 0 then c else
      f ()

  let compare t1 t2 =
    let<> () = VarAtom.compare t1.atom t2.atom in
    let<> () = VarInt.compare t1.int t2.int in
    let<> () = VarChar.compare t1.char t2.char in
    let<> () = VarUnit.compare t1.unit t2.unit in
    let<> () = VarProduct.compare t1.product t2.product in
    let<> () = VarProduct.compare t1.arrow t2.arrow in
    0
  let h v x = v + ((x lsl 5) - x)

  let hash t =
    h (VarAtom.hash t.atom) 0
    |> h (VarInt.hash t.int)
    |> h (VarChar.hash t.char)
    |> h (VarUnit.hash t.unit)
    |> h (VarProduct.hash t.product)
    |> h (VarProduct.hash t.arrow)

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

and VarProduct : (Sigs.Bdd with type atom = Var.t and type leaf = Product.t) =
  Bdd.Make (Var) (Product)

include Descr

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

let make () = node empty

let def n t = n.descr <- t

let var_product n1 n2 =
  VarProduct.(leaf (Product.atom (n1, n2)))
let product n1 n2 =
  { empty with product = var_product n1 n2}

let arrow n1 n2 =
  { empty with arrow = var_product n1 n2}

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
