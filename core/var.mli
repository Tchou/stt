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

val make : ?kind:kind -> string -> t
val dump : Format.formatter -> t -> unit

module Set : sig
  include Stdlib.Set.S with type elt = t 
  include Base.Common.T with type t := t
end

val name_not_in : Set.t -> string
