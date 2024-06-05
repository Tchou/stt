module type S = sig

  type lt
  (** The letter type *)
  type t
  (** The automaton type *)

  module R : Regexp.S
  (** The regexp module *)
  type regexp
  (** The regexp type *)

  val empty : t
  (** The empty automaton *)

  val add_state : t -> int -> t
  (** [add_state automaton state] adds [state] in [automaton]. 
    If it's already a state in [automaton], does nothing *)
  val add_states : t -> int list -> t
  (** [add_states automaton states] adds several states in [automaton] *)
  val add_trans : t -> int -> lt -> int -> t
  (** [add_trans automaton state1 letter state2] adds a transition labelled [letter] between [state1] and [state2] in [automaton].

    Raises : [state1] and [state2] must be [automaton]'s states.
    Raises : [l] must be in the [automaton]'s alphabet.

    If the transition is already in [automaton], does nothing *)
  val add_transitions : t -> (int * lt * int) list -> t
  (** [add_transitions automaton transitions] adds several transitions in [automaton] *)
  val add_start : t -> int -> t
  (** [add_start automaton state] sets [state] as a start state in [automaton].
    If it's already the case, does nothing

    Raise : [state] must be an [automaton]'s state *)
  val add_starts : t -> int list -> t
  (** [add_starts automaton states] sets several states as start states in [automaton] *)
  val add_end : t -> int -> t
  (** [add_end automaton state] sets [state] as an end state in [automaton].
    If it's already the case, does nothing

    Raise : [state] must be an [automaton]'s state *)
  val add_ends : t -> int list -> t
  (** [add_ends automaton states] sets several states as end states in [automaton] *)

  val check_word : t -> lt list -> bool
  (** [check_word automaton word] checks if [word] is recognized by [automaton] *)
  val to_regex_my : t -> regexp
  (** [to_regex_my automaton] returns the regex representing [automaton] using the McNaughton-Yamada method. 
    The returned value might be unsimplified *)
end

module Make (Lt : Letter.Letter) : S with type lt = Lt.t 
                                     and module R = Regexp.Make(Lt) 
                                     and type regexp = Regexp.Make(Lt).t_simp