open Base

include Sigs.Interval with type t = private (Uchar.t * Uchar.t) list
                       and type elem = Uchar.t
val pp_char : Format.formatter -> Uchar.t -> unit