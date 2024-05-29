module type Symbol = sig

  (* The symbol type *)
  type t

  (* [compare s1 s2] returns a negative number if [s1] < [s2], 0 if [s1] = [s2] 
      or a positive number otherwise *) 
  val compare : t -> t -> int
  (* [to_string s] cast the symbol [s] into a string *)
  val pp : t -> string

end

module type Letter = sig

  (* Tje symbol type *)
  type symbol
  (* A letter is a symbol, with an epsilon *)
  type t

  (* [compare l1 l2] returns a negative number if [l1] < [l2], 0 if [l1] = [l2] 
      or a positive number otherwise *) 
  val compare : t -> t -> int
  (* [to_string l] casts the letter [l] into a string *)
  val pp : t -> string

  (* The epislon value *)
  val epsilon : t
  (* [is_epsilon l] checks if [l] is epsilon *)
  val is_epsilon : t -> bool
  
  (* [get l] returns the correponding symbol in the letter [l] : if it's epsilon,
      it returns [None], otherwise it returns [Some s], where [s] is the associated symbol *)
  val get : t -> symbol option
  (* [symbol s] creates a letter associated with the symbol [s] *)
  val symbol : symbol -> t

end

module AddEpsilon(Sym : Symbol) : Letter with type symbol = Sym.t