include Sigs.Interval with type elem = Z.t

val left : elem -> t
val right : elem -> t

include Sigs.Kind with type t := t and type atom = Z.t
