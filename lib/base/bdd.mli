module Make (X : Common.T) (L : Sigs.PreSet) :
  Sigs.Bdd with type atom = X.t and type leaf = L.t

module MakeLevel2 (X : Common.T) (L : Sigs.Bdd) : sig
  include Sigs.Bdd with type atom = X.t and type leaf = L.t

  val full_dnf :
    t -> ((atom list * atom list) * (L.atom list * L.atom list)) Seq.t
end
