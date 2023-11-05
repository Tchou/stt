module Make (X : Common.T) (L : Sigs.Set) :
  Sigs.Bdd with type atom = X.t and type leaf = L.t
(** A functor to make Binary Decision Diagrams from the given
    [atom] and [leaf] components.
*)

module MakeLevel2 (X : Common.T) (L : Sigs.Bdd) : Sigs.Bdd2
  with
    module Leaf = L
   and type atom = X.t
(** A functor to make Binary Decision Diagrams whose [leaf] elements
    are themselves BDD.
*)

