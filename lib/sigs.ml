module type Interval = sig
  include Common.T

  type elem

  val empty : t
  val is_empty : t -> bool
  val any : t
  val is_any : t -> bool
  val cup : t -> t -> t
  val cap : t -> t -> t
  val diff : t -> t -> t
  val neg : t -> t
  val range : elem -> elem -> t
  val intersect : t -> t -> bool
  val sample : t -> elem option
  val mem : elem -> t -> bool
end

module type Kind = sig
  include Common.T

  val name : string

  type atom

  (* Set theoretic operations *)
  val empty : t
  val any : t
  val cup : t -> t -> t
  val cap : t -> t -> t
  val neg : t -> t
  val diff : t -> t -> t
  val atom : atom -> t
  val is_empty : t -> bool
  val is_any : t -> bool
end
