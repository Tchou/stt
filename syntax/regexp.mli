module type Letter = sig

  type t
  (** A letter is a symbol, with an epsilon *)

  val compare : t -> t -> int
  (** [compare l1 l2] returns a negative number if [l1] < [l2], 0 if 
    [l1] = [l2] or a positive number otherwise *) 
  val pp : Format.formatter -> t -> unit
  (** [pp fmt l] casts the letter [l] into a string *)

  val epsilon : t
  (** The epsilon value *)
  val is_epsilon : t -> bool
  (** [is_epsilon l] checks if [l] is epsilon *)
  
end

module type S = sig

  type lt
  (** The letter type *)

  type t_simp
  (** A simple implementation of a regex *)
  type t_ext
  (** An extended version of the regex (with the option and plus) *)

  val empty : t_simp
  (** The empty regex *)
  val is_empty : t_simp -> bool
  (** [is_empty r] checks if the regexp is the empty (not the 
    empty word : ɛ ≠ ∅) *)

  val letter : lt -> t_simp
  (** [letter l] creates a regex with the letter [l] *)
  val concat : t_simp -> t_simp -> t_simp
  (** [concat r1 r2] creates a regex by concatenating [r1] with [r2] *)
  val union : t_simp -> t_simp -> t_simp
  (** [union r1 r2] creates a regex by uniting [r1] with [r2] *)
  val star : t_simp -> t_simp
  (** [star r] creates a regex with the Kleene star *)

  val simp_to_ext : t_simp -> t_ext
  (** [simp_to_ext r] extends the simple regex [r], in order to simplify 
    or print it *)

  val to_string : (Format.formatter -> lt -> unit) -> t_ext -> string
  (** [to_string pp_lt r] cast the regex [r] into a string, using 
    [pp_lt] to cast the letter*)

  val simplify : t_ext -> t_ext
  (** [simplify r] normalizes and simplfies the regex [r] as much as possible *)

end

module Make (Lt : Letter) : S with type lt = Lt.t