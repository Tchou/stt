(** Type substitutions *)

type 'a t
(** The type of type substitutions. *)

val add : Var.t -> 'a -> 'a t -> 'a t
(** [add var x s] adds the [var] to the domain of [s], associating
    value [x].
    @raises Invalid_argument if [var] is already in the domain of [s]
*)

val to_list : 'a t -> (Var.t * 'a) list
(** [to_list subst] returns the substitution as an association list. *)

val of_list : (Var.t * 'a) list -> 'a t
(** [of_list lst] returns the substitution corresponding to the given
    association list.
    @raises Invalid_argument if [lst] contains duplicate keys. *)

val domain : 'a t -> Var.Set.t
(** [domain subst] returns the domain of the substitution [subst]. *)

val apply : Typ.t t -> Typ.t -> Typ.t
(** [apply subst t] applies the given type susbtitution to the type t. *)

val apply_gen : (Var.t -> Typ.t) -> Typ.t -> Typ.t
(** [apply f t] applies the type susbtitution given as as function,
    to the type t. *)

val refresh : Typ.t -> Typ.t * Typ.t t
(** [refresh t] substitutes all variables of [t] with fresh variables.
    The function returns the new type and the substitution.
*)
