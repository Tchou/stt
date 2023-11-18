module Make (X : Common.T) (L : Sigs.Set) : Sigs.Bdd
  with type atom = X.t
    and type Leaf.t = L.t
    and type Leaf.elem = L.elem
(** A functor to make Binary Decision Diagrams from the given
    [atom] and [leaf] components.
*)

module MakeLevel2 (X : Common.T) (L : Sigs.Bdd) : Sigs.Bdd2
  with type Leaf.t = L.t
   and type Leaf.elem = L.elem
   and type LeafBdd.t = L.t
   and type LeafBdd.atom = L.atom
   and type atom = X.t
(** A functor to make Binary Decision Diagrams whose [leaf] elements
    are themselves BDD.
*)

