(** Set-theoretic types and their operations. *)


(** {1:basic Basics} *)

type t
(** [t] represents a set-theoretic type. It is the disjoint unions
    of the various kinds defined below. Basic operations of the signature
    {!Base.Common.T} are implemented. Note :

    - {!equal} is the structural equality on the internal representation of types,
      not semantic equality
    - {!pp} prints the internal representation of the type and is not a pretty-printer.

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

module type Basic = Base.Sigs.Bdd with type atom = Var.t
module type Constr = Base.Sigs.Bdd2 with type atom = Var.t

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
    
module Singleton : sig
  val atom : string -> t
  val int : Z.t -> t
  val char : Uchar.t -> t
  val unit : t
end
(** Utility module ton construct singleton types. *)

val node : t -> Node.t
(** [node t] creates a reference to the type [t]. *)

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

