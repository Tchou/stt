type 'a t
(** The type of type substitutions. *)

val add : Var.t -> 'a -> 'a t -> 'a t
(** [add var x s] adds the [var] to the domain of [s], associating
    value [x].
    @raises Invalid_argument if [var] is already in the domain of [s]
    *)

val to_list : 'a t -> (Var.t * 'a) list
val of_list : (Var.t * 'a) list -> 'a t
val apply : Typ.t t -> Typ.t -> Typ.t
val vars : Typ.t -> Var.Set.t * Var.Set.t
