open Format

type pr = formatter -> unit
type single =
  | Singleton of pr
  | Range of pr * pr
(** The atomic printer type *)

type 'a t = bool * ('a * single) list
(** The type in order to print union 

    The boolean specifies if it's a diff or not
    We keep the 'a with its associated single printer*)

val pr_string : string -> formatter -> unit
(** [pr_string s fmt] prints the string [s] in [fmt] *)

val pp_single : formatter -> single -> unit 
(** [pp_single fmt s] prints the single [s] in [fmt] *)

val pp : ?pp_any:(formatter -> unit) -> ?pp_empty:(formatter -> unit)
         -> formatter -> 'a t -> unit
(** [pp ?pp_any ?pp_empty fmt t] prints [t] in [fmt]

    If t is (false, []), then we use [pp_empty]
    If t is (true, []), then we use [pp_any]
    Otherwise, we print the boolean and the list seperated with a "," *)