module type Symbol = sig

  type t
  (** The symbol type *)

  val compare : t -> t -> int
  (** [compare s1 s2] returns a negative number if [s1] < [s2], 0 if [s1] = [s2] 
      or a positive number otherwise *) 
  val pp : t -> string
  (** [pp s] cast the symbol [s] into a string *)

end

module type Letter = sig

  type symbol
  (** The symbol type *)
  type t
  (** A letter is a symbol, with an epsilon *)

  val compare : t -> t -> int
  (** [compare l1 l2] returns a negative number if [l1] < [l2], 0 if [l1] = [l2] 
      or a positive number otherwise *) 
  val pp : t -> string
  (** [pp l] casts the letter [l] into a string *)

  val epsilon : t
  (** The epsilon value *)
  val is_epsilon : t -> bool
  (** [is_epsilon l] checks if [l] is epsilon *)

  val get : t -> symbol option
  (** [get l] returns the correponding symbol in the letter [l] : if it's epsilon,
      it returns [None], otherwise it returns [Some s], where [s] is the associated symbol *)
  val symbol : symbol -> t
  (** [symbol s] creates a letter associated with the symbol [s] *)

end

module AddEpsilon(Sym : Symbol) : Letter with type symbol = Sym.t