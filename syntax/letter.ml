module type Letter = sig
  type t
  (** A letter is a symbol, with an epsilon *)

  val compare : t -> t -> int
  (** [compare l1 l2] returns a negative number if [l1] < [l2], 0 if 
    [l1] = [l2] or a positive number otherwise *) 
  val pp : t -> string
  (** [pp l] casts the letter [l] into a string *)

  val epsilon : t
  (** The epsilon value *)
  val is_epsilon : t -> bool
  (** [is_epsilon l] checks if [l] is epsilon *)
end