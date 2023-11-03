(** Set-theoretic types and their operations. *)


(** {1:basic Basics} *)

type t
type descr = t

(** [t] represents a set-theoretic type. It is the disjoint unions
    of the various kinds defined below. Basic operations of the signature
    {!Base.Common.T} are implemented. Note :

    - {!equal} is the structural equality on the internal representation of types,
      not semantic equality
    - {!pp} prints the internal representation of the type and is not a pretty-printer.

    [descr] is an alias for [t] that is used in nested modules below that also define their
    own type [t] (e.g. {!Basic}).
*)


include Base.Common.T with type t := t

(** A {!Node.t} is a reference to a type. It can be seen as a recursive type
    variable. *)
module Node : Base.Common.T

(** {1:comp Components}

    Components represents the various type constructors that constitute a
    (set-theoretic) type. All components are disjoint from one another. Each
    component conceptually represents a DNF of (polymorphic) type variables and
    atoms.

    {!Basic} components have the structure of a {!Base.Sigs.Bdd} whose atoms are variables
    (see {!Var}) and leaves are basic types (integers, characters, atoms, unit).

    {!Constr} components have the structure of a two-level {!Base.Sigs.Bdd2}. The atoms of the
    first level are variables and the second level, that is, the leaves of the first levels
    is a BDD whose atoms are type constructors : products, arrows, and so on.
*)

module type Basic = sig
  include Base.Sigs.Bdd with type atom = Var.t
  val get : descr -> t
  val set : t -> descr -> descr
end
module type Constr = sig
  include Base.Sigs.Bdd2 with type atom = Var.t
  val get : descr -> t
  val set : t -> descr -> descr
end

val num_components : int
(** The number of components in a type. *)

type component =
    Basic : (module Basic) -> component
  | Constr : (module Constr) -> component
(** A type representing a component as a first class module. *)

val all_components : component list
(** The list of all components of a type. *)

(** {2:basic-comp Basic components}*)

module VarAtom : Basic with type leaf = Atom.t
module VarInt : Basic with type leaf = Int.t
module VarChar : Basic with type leaf = Char.t
module VarUnit : Basic with type leaf = Unit.t
(** The basic components: Atoms, Integers, Characters and Unit *)

(** {2:constr-comp Constructor components}*)

module Product : Base.Sigs.Bdd with type atom = Node.t * Node.t
(** A BDD whose atoms are pairs of type references, representing a DNF
    a products *)

module VarProduct : Constr with module Leaf = Product
module VarArrow : Constr with module Leaf = Product
(** The constr components. Arrows have the same internal representation as
    products but a different interpretation.*)


(** {2:comp-get Component accessors }*)

module Get : sig
  val atom : t -> VarAtom.t
  (** [atom t] returns the atom component of a type [t]. *)

  val int : t -> VarInt.t
  (** [int t] returns the integer component of a type [t]. *)

  val char : t -> VarChar.t
  (** [char t] returns the char component of a type [t]. *)

  val unit : t -> VarUnit.t
  (** [unit t] returns the unit component of a type [t]. *)

  val product : t -> VarProduct.t
  (** [product t] returns the product component of a type [t]. *)

  val arrow : t -> VarProduct.t
  (** [arrow t] returns the arrow component of a type [t]. *)
end
(** Allows one to retrieve the component of a type as a BDD. *)

module Set : sig
  val atom : VarAtom.t -> t -> t
  (** [atom t] updates the atom component of a type [t]. *)

  val int : VarInt.t -> t -> t
  (** [int t] updates the integer component of a type [t]. *)

  val char : VarChar.t -> t -> t
  (** [char t] updates the char component of a type [t]. *)

  val unit : VarUnit.t -> t -> t
  (** [unit t] updates the unit component of a type [t]. *)

  val product : VarProduct.t -> t -> t
  (** [product t] updates the product component of a type [t]. *)

  val arrow : VarProduct.t -> t -> t
  (** [arrow t] updates the arrow component of a type [t]. *)
end
(** Allows one to update the component of a type with a BDD. *)


(** {1:type-alg Type algebra }*)

val empty : t
(** The empty type 𝟘. *)

val any : t
(** The top type 𝟙. *)

val cup : t -> t -> t
(** [cup t1 t2] returns the union of [t1] and [t2]. *)

val cap : t -> t -> t
(** [cap t1 t2] returns the intersection of [t1] and [t2]. *)

val diff : t -> t -> t
(** [diff t1 t2] returns the difference of [t1] and [t2]. *)

val neg : t -> t
(** [neg t] returns the negation of [t]. *)

val product : Node.t -> Node.t -> t

(** [product n1 n2] creates the product type of the two type references passed
    as argument. Wrapping a type in a reference before passing it to a
    constructor ensures the contractivity of (potentialy recursive) types.
    Type references can be created with {!make} and {!node}.
*)

val arrow : Node.t -> Node.t -> t
(** Same as [product] but construct an arrow type. *)

val var : Var.t -> t
(** [var v] creates a type containing only the polymorphic variable [v]. *)

module Singleton : sig
  val atom : Base.Hstring.t -> t
  val int : Z.t -> t
  val char : Uchar.t -> t
  val unit : t
end
(** Utility module ton construct singleton types. *)

val node : t -> Node.t
(** [node t] creates a reference to the type [t]. *)

val descr : Node.t -> t
(** [descr n] dereferences the node [n]. *)

val make : unit -> Node.t
(** [make ()] creates an unassigned type reference. *)

val def : Node.t -> t -> unit
(** [def n t] assign the type [t] to a dangling reference. This can be used
    to create a recursive type:
    {[
      let nil = Typ.Singleton.atom "nil"
      let x = Typ.make ()
      let p = Typ.(product (node any) x)
      let any_list = Typ.cup p nil
      let () = Typ.def x any_list
      (* any_list is now the type [Any*] *)
    ]}
    It is an error to call [def] on an already assigned type reference.
*)

(** {1:iter Iterators}
*)
type ('var, 'atom, 'int, 'char, 'unit, 'product, 'arrow) op =
  {
    var : 'var;
    atom : 'atom;
    int : 'int;
    char : 'char;
    unit : 'unit;
    product : 'product;
    arrow : 'arrow;
  }
(** Type [op] holds together operations on the components of a type. *)

val fold : op:((bool -> 'acc -> Var.t -> 'acc),
               ('acc -> Atom.t -> 'acc),
               ('acc -> Int.t -> 'acc),
               ('acc -> Char.t -> 'acc),
               ('acc -> Unit.t -> 'acc),
               (bool -> 'acc -> (Node.t*Node.t) -> 'acc),
               (bool -> 'acc -> (Node.t*Node.t) -> 'acc)) op ->
  cup:('acc -> 'acc -> 'acc) ->
  empty:'acc ->
  any:'acc ->
  t -> 'acc
(** [fold ~op ~cup ~cap ~diff ~empty ~any t] folds the functions of
    [op] over the structure of [t], following its DNF.
    For instance, for integer components, assuming the DNF of is equivalent:
    {[
      t_int :=  [ ((pv1, nv1), i1);
                  ((pv2, nv2), i2);
                  ...
                    ((pvk, nvk), ik)]
    ]}
    with [pvi] intersection of positive variables, [nvi] intersection on negative variables
    and [ii] integers, then [fold] computes for this component:
    {[
      List.fold_left (fun acc_u ((pvi, nvi), ii) ->
          cup acc_u (
            let acc_i = List.fold_left (op.var true) any pvi in
            let acc_i = List.fold_left (op.var false) acc_i nvi in
            op.int acc_i ii
          ) empty t_int
    ]}
*)

val iter : op:((bool -> Var.t -> unit),
               (Atom.t -> unit),
               (Int.t -> unit),
               (Char.t -> unit),
               (Unit.t -> unit),
               (bool -> (Node.t*Node.t) -> unit),
               (bool -> (Node.t*Node.t) -> unit)) op ->
  t ->
  unit
(** [iter ~op t] traverses the structure of [t] and applies the appropriate
    functions in [op] on each internal atom.
*)

val ignore_iter_op : ((bool -> Var.t -> unit),
                      (Atom.t -> unit),
                      (Int.t -> unit),
                      (Char.t -> unit),
                      (Unit.t -> unit),
                      (bool -> (Node.t*Node.t) -> unit),
                      (bool -> (Node.t*Node.t) -> unit)) op
(** Operation for [iter] that ignores every component. *)

val map : op:((Var.t -> t),
              (Atom.t -> Atom.t),
              (Int.t -> Int.t),
              (Char.t -> Char.t),
              (Unit.t -> Unit.t),
              ((Node.t*Node.t) -> (Node.t*Node.t)),
              ((Node.t*Node.t) -> (Node.t*Node.t))
             ) op
  -> t -> t
(** [map ~op t] computes a new type [t'] by applying the transformations in [op]
      to each internal atom. Each atom must be transformed into one of the same
      kind, except for variables which can be transformed in any type.
*)

val id_map_op : ((Var.t -> t),
                 (Atom.t -> Atom.t),
                 (Int.t -> Int.t),
                 (Char.t -> Char.t),
                 (Unit.t -> Unit.t),
                 ((Node.t*Node.t) -> (Node.t*Node.t)),
                 ((Node.t*Node.t) -> (Node.t*Node.t))
                ) op
(** Operation for [map] that performs the identity function for each internal
    component.
*)

(** {1:vars Polymorphic variables } *)

val vars : t -> Var.Set.t * Var.Set.t
(** [vars t] returns the sets [(co, contra)] of variables in covariant
    and contravariant positions. Invariant variables can be computed with:
    [Var.Set.inter co contra].
*)

val toplevel_vars : t -> Var.Set.t * Var.Set.t
(** [toplevel_vars] is like [vars] but does not recurse below constructors.
*)

val single_var : t -> (Var.t * bool) option
(** [single_var t] returns [None] if [t] is not a single variable or a negation
      of a single variable. Otherwise it returns [Some(v, b)] where [v] is the
      variable and [b] denotes the polarity of the variable.
*)

(** {1:sub Subtyping} *)

val is_empty : t -> bool
(** [is_empty t] is [true] if and only if [t] is empty. *)

val subtype : t -> t -> bool
(** [subtype s t] is [true] if and only if [s] ≤ [t]. *)

val equiv : t -> t -> bool
(** [equiv s t] is [true] if and only if [s] ≤ [t] and [t] ≤ [s]. *)