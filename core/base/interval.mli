module type V = sig
  include Common.T

  val min : t
  val max : t
  val succ : t -> t
  val pred : t -> t
end

module Make (X : V) :
  Sigs.Interval
    with type t = Common.Pair(X)(X).t list
     and type elem = X.t
