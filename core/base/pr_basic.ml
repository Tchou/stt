type pr = Format.formatter -> unit

type single =
  | Singleton of pr
  | Range of pr * pr

type 'a t = bool (* [true] if it's a diff like Enum\{`A|...}, 
                    [false] otherwise like `True|`False *)
            * ('a * single) list