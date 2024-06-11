type pr = Format.formatter -> unit

type single =
  | Singleton of pr
  | Range of pr * pr

type 'a t = bool * ('a * single) list