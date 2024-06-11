type t =
  | Item of (Format.formatter -> unit)
  | Neg of t
  | Cap of t list
  | Cup of t list
  | Dif of t list