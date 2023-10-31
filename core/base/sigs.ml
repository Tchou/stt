module type Hcons = sig
  type v
  include Common.T
  val make : v -> t
  val id : t -> int
  val (!!) : t -> v
end

module type PreSet = sig
  include Common.T

  val empty : t
  val is_empty : t -> bool
  val any : t
  val is_any : t -> bool
  val neg : t -> t
  val cup : t -> t -> t
  val cap : t -> t -> t
  val diff : t -> t -> t
end

module type Set = sig
  include PreSet

  val name : string

  type elem

  val singleton : elem -> t
  val intersect : t -> t -> bool
  val sample : t -> elem option
  val mem : elem -> t -> bool
end

module type Interval = sig
  include Common.T
  include Set with type t := t

  val range : elem -> elem -> t
end

module type FiniteCofinite = sig
  include Common.T
  include Set with type t := t

  val is_finite : t -> bool
end

module type Bdd = sig
  include PreSet

  type atom
  type leaf

  module Conj : Common.T with type t = (atom list * atom list) * leaf
  module Disj : Common.T with type t = Conj.t list 
  val atom : atom -> t
  val leaf : leaf -> t
  val dnf : t -> Conj.t Seq.t

  val map : atom:(atom -> t) -> leaf:(leaf -> leaf) -> t -> t
  val fold :
    atom:(bool -> atom -> 'elem) ->
    cup:('disj -> 'line -> 'disj) ->
    cap:('conj -> 'elem -> 'conj) ->
    diff:('conj -> 'elem -> 'conj) ->
    leaf:(leaf -> 'conj -> 'line) ->
    empty:'disj ->
    any :'conj ->
    t -> 'disj

end

module type Bdd2 = sig
  module Leaf : Bdd
  include Bdd with type leaf = Leaf.t
  val full_dnf :
    t -> ((atom list * atom list) * (Leaf.atom list * Leaf.atom list)) Seq.t
end