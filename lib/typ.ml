open Base

type ('atom, 'int, 'char, 'unit, 'product, 'arrow) descr = {
  atom : 'atom;
  int : 'int;
  char : 'char;
  unit : 'unit;
  product : 'product;
  arrow : 'arrow;
}

type 'a node = {
  mutable descr : 'a;
  id : int;
}

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

  let compare t1 t2 =
    let c = VarAtom.compare t1.atom t2.atom in
    if c <> 0 then c
    else
      let c = VarInt.compare t1.int t2.int in
      if c <> 0 then c
      else
        let c = VarChar.compare t1.char t2.char in
        if c <> 0 then c
        else
          let c = VarUnit.compare t1.unit t2.unit in
          if c <> 0 then c
          else
            let c = VarProduct.compare t1.product t2.product in
            if c <> 0 then c
            else
              let c = VarProduct.compare t1.arrow t2.arrow in
              c

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
