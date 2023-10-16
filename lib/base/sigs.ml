module type PreSet = sig
  include Common.T

  val empty : t
  val is_empty : t -> bool
  val any : t
  val is_any : t -> bool
  val cup : t -> t -> t
  val cap : t -> t -> t
  val neg : t -> t
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
  include Common.T

  type atom
  type leaf

  val empty : t
  val any : t
  val is_empty : t -> bool
  val is_any : t -> bool
  val atom : atom -> t
  val leaf : leaf -> t
  val cup : t -> t -> t
  val cap : t -> t -> t
  val diff : t -> t -> t
  val neg : t -> t
end
