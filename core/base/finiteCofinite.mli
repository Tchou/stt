module Make : functor (X : Common.T) -> sig
  module S : Set.S with type elt = X.t

  include Sigs.FiniteCofinite
    with type elem = S.elt
     and type t =
           [ `Finite of S.t
           | `Cofinite of S.t
           ]
end
