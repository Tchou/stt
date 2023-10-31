module type Hcons = sig
  type v
  include Common.T
  val make : v -> t
  val id : t -> int
  val (!!) : t -> v
end

module type Poset = sig
  (** The signature of posets that can be {!empty} or full ({!any}) together
      with set theoretic operations.
  *)

  include Common.T
  val empty : t
  (** The empty poset. *)

  val is_empty : t -> bool
  (** [is_empty s] is [true] if and only if the [s] has no elements. *)

  val any : t
  (** The full poset. *)

  val is_any : t -> bool
  (** [is_any s] is [true] if and only if the [s] has all the elements. *)

  val neg : t -> t
  (** [neg s] is the complement of [s]. *)

  val cup : t -> t -> t
  (** [cup s1 s2] is the union of [s1] and [s2]. *)

  val cap : t -> t -> t
  (** [cap s1 s2] is the intersection [s1] and [s2]. *)

  val diff : t -> t -> t
  (** [diff s1 s2] is the difference between [s1] and [s2]. It should be equal
        to [cap s1 (neg s2)] (but can be implemented more efficiently). *)
end

module type Set = sig
  (** A full (po)Set signature with function to build sets. *)

  include Poset

  val name : string
  (** The name of the set, for pretty-printing purpose. *)

  type elem
  (** The type of elements of the sets. *)

  val singleton : elem -> t
  (** [singleton e] is the set containing only [e]. *)

  val intersect : t -> t -> bool
  (** [intersect s1 s2] returns true if and only if [s1] and [s2] have a
        non-empty intersection. It is equivalent to [not (is_empty (cap s1 s2))]
        (but can be implemented more efficiently).*)

  val sample : t -> elem option
  (** [sample s] returns [None] if [s] is empty, and [Some e] otherwise,
      where [e] is an element of the set [s]. *)

  val mem : elem -> t -> bool
  (** [mem e s] returns [true] if and only if [e] is an element of [s]. *)
end

module type Interval = sig
  (** Intervals are sets that represents unions
      of ranges of elements.
  *)

  include Common.T
  include Set with type t := t

  val range : elem -> elem -> t
end

module type FiniteCofinite = sig
  (** The signature of sets that are either finite or co-finite (that is, sets
      the complement of which are finite).
  *)

  include Common.T
  include Set with type t := t

  val is_finite : t -> bool
  (** [is_finite s] is [true] if and only if [s] is finite. *)
end

module type Bdd = sig
  (** Binary Decision Diagrams. *)

  (** Binary Decision Diagrams are used to represent
      sets with a particular structure. Given a type {!type:atom} of
      elements and a type {!type:leaf} of objects, a BDD may
      represent the set in Disjunctive Normal Form:

       x{_1{_ 1}}∩…∩ x{_1{_ n}}∩ l{_1}

       ∪ …

       ∪ x{_m{_ 1}}∩…∩ x{_m{_ n}}∩ l{_m}

      where the x{_ i} can be positive atoms (a{_ i}, the atom is
      explicitely in the set) or negative atoms (¬a{_ i}, the atom is
      explicitely not in the set).
      An intersection x{_i{_ 1}}∩…∩ x{_i{_ 1}} is a line,
      x{_m{_ 1}}∩…∩ x{_m{_ n}}∩ l{_m} is a conjunction and the whole
      BDD a disjunction.
  *)

  include Poset

  type atom
  (** The type of [atom] elements. *)

  type leaf
  (** The type of [leaf] objects. *)

  module Conj : Common.T with type t = (atom list * atom list) * leaf
  (** A Utility module to represent conjunctions. *)

  module Disj : Common.T with type t = Conj.t list
  (** A Utility module to represent disjunctions. *)

  val atom : atom -> t
  (** [atom a] creates a BDD that only contains [a]. *)

  val single_atom : t -> (atom*bool) option
  (** [single_atom t] returns [Some(a, b)] if [t] consists of
      a single atom [a]. The boolean [b] is [true] if [a] is
      a positive atom and [false] if it is a negative atom.

      The function returns [None] in all other cases.  *)

  val leaf : leaf -> t
  (** [leaf l] creates a BDD that only contains the leaf [l]. *)

  val dnf : t -> Conj.t Seq.t
  (** [dnf t] returns the sequence of conjunctions dans constitute [t]. The
      sequence is built lazily and memoized, so iterating the sequence twice
      does not recompute the whole DNF. Note however that the DNF can be
      exponential in the size of [t].
  *)

  val map : atom:(atom -> t) -> leaf:(leaf -> leaf) -> t -> t
  (** [map ~atom ~leaf t] computes a new BDD whose elements are obtained
      by calling [atom] and [leaf] on atoms and leaf elements.
  *)

  val fold :
    atom:(bool -> 'line -> atom -> 'line) ->
    leaf:('line -> leaf -> 'conj) ->
    cup:('disj -> 'conj -> 'disj) ->
    empty:'disj ->
    any :'line ->
    t -> 'disj
    (** [fold ~atom ~cup ~leaf ~empty ~any t] folds the functions over the structure of [t].
        If [t] is:

            x{_1{_ 1}}∩…∩ x{_1{_ n}}∩ l{_1}

         ∪ …

         ∪ x{_m{_ 1}}∩…∩ x{_m{_ n}}∩ l{_m}

        then [fold ~atom ~cup ~leaf ~empty ~any t] computes:
        {[
          let acc = leaf (atom bn1 ... (atom b21 (atom b11 x11 any) x21) xn1) l1 in
          let acc = cup acc (leaf (atom bn2 ... (atom b12 any x12) xn2) l2 ) in
          ...
            let acc = cup acc (leaf (atom bnm ... (atom b1m any x1m) xnm) lm) in
            acc
        ]}
        where [bij] is [true] if x{_i{_ j}}≡a{_i{_ j}} and [false] if
        x{_i{_ j}}≡¬a{_i{_ j}}
    *)

end

module type Bdd2 = sig
  (** Two-level Binary Decision Diagrams. *)

  (** Binary Decision Diagrams for which the {!type:leaf} is also a BDD.
      This corresponds to:

           x{_1{_ 1}}∩…∩ x{_1{_ n}}∩ l{_1{_1}}∩…∩ l{_p{_ 1}}

       ∪ …

       ∪ x{_m{_ 1}}∩…∩ x{_m{_ n}}∩ l{_m{_1}}∩…∩ l{_m{_ p}}

       where l{_i{_j}} can be a positive or negative {!Leaf.t} atom.

  *)

  module Leaf : Bdd
  (** The module representing leaf elements. It is a BDD whose own [leaf] elements
      can be ignored (typically a singleton which can be either [empty] of [any],
      isomorphic to a Boolean value). *)

  include Bdd with type leaf = Leaf.t
  val full_dnf :
    t -> ((atom list * atom list) * (Leaf.atom list * Leaf.atom list)) Seq.t
    (** Returns the sequence of conjunction of the BDD. The leaf elements are themselves
        unfolded, and the elements of type [Leaf.leaf] are ignored. *)

end