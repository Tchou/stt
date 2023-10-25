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

module Node : Base.Common.T

(** {1:kinds Kinds} *)
module type VarBdd = Base.Sigs.Bdd with type atom = Var.t

module VarAtom : VarBdd with type leaf = Atom.t
module VarInt : VarBdd with type leaf = Int.t
module VarChar : VarBdd with type leaf = Char.t
module VarUnit : VarBdd with type leaf = Unit.t
module Product : Base.Sigs.Bdd with type atom = Node.t * Node.t
module VarProduct : VarBdd with type leaf = Product.t


val empty : t
val any : t
val node : t -> Node.t
val make : unit -> Node.t
val def : Node.t -> t -> unit
val cup : t-> t -> t
val cap : t -> t ->t
val diff : t -> t -> t
val neg : t -> t

val product : Node.t -> Node.t -> t
val arrow : Node.t -> Node.t -> t

module Singleton : sig
  val atom : string -> t
  val int : Z.t -> t
  val char : Uchar.t -> t
  val unit : t
end
