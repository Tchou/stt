type t = 
  | Item of (formatter -> unit)
  | Neg of t
  | Union of t list
  | Inter of t list
  | Diff of t list

Flatten les list
Simplifier (sauf diff : pas toucher)
  => Simplifier comment????????? Oublire

Avec hypothèse que t.Item print que des types "atomiques"