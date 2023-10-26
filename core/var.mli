type kind =
  [ `generated
  | `user
  ]

type t = private {
  id : int;
  name : string;
  kind : kind;
}

include Base.Common.T with type t := t

val uid : unit -> int
val make : ?kind:kind -> string -> t
val dump : Format.formatter -> t -> unit

module Set : Stdlib.Set.S with type elt = t

val name_not_in : Set.t -> string
