module type Set = sig
  include Common.T

  val name : string

  type elem

  (* Set theoretic operations *)
  val empty : t
  val is_empty : t -> bool
  val any : t
  val is_any : t -> bool
  val singleton : elem -> t
  val cup : t -> t -> t
  val cap : t -> t -> t
  val neg : t -> t
  val diff : t -> t -> t
  val intersect : t -> t -> bool
  val sample : t -> elem option
  val mem : elem -> t -> bool
end

module type Interval = sig
  include Common.T
  type elem
  include Set with type t := t and type elem := elem

  val range : elem -> elem -> t
end
