module Make (X1 : Sigs.Set) (X2 : Sigs.Set ) : sig

  type simple = (X1.t * X2. t) list Seq.t
  (** The type of a simple product decomposition which represents
      an arbitrary union of union of products.*)

  type normal = (X1.t * X2.t) list
  (** The type of a normalized product decomposition which
      represent a union of products that are pairwise disjoints on their first component.
  *)

  type dnf = ((X1.t * X2.t) list * (X1.t * X2.t) list) Seq.t
  (** The type of a DNF of products, which is a union of intersections
      of positive products and negative products.
  *)

  val simple : dnf -> simple
  (** [simple dnf] simplifies a DNF of products into a union of products,
      eliminating the negative parts.
  *)

  val normal : dnf -> normal
  (* [normal dnf] normalizes a DNF of products into a union of products
     disjoints on their first component.
  *)
end
