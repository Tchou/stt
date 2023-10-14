open Common

include Sigs.Interval with type t = private (Uchar.t * Uchar.t) list
                       and type elem = Uchar.t
include Sigs.Kind with type atom = Common.Uchar.t and type t := t
