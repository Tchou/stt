module Make (X : Common.T) (L : Sigs.PreSet) :
  Sigs.Bdd with type atom = X.t and type leaf = L.t

module MakeLevel2 (X : Common.T) (L : Sigs.Bdd) : Sigs.Bdd2
  with
    module Leaf = L
   and type atom = X.t

