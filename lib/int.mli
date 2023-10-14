open Utils
include Sigs.Interval with type elem = Z.t

val left : elem -> t
val right : elem -> t
