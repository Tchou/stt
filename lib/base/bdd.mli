module Make (X : Common.T) (L : Sigs.PreSet) :
  Sigs.Bdd with type atom = X.t and type leaf = L.t
