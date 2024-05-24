type lt
(** The letter type *)

type t_simp
(** A simple implementation of a regex *)
type t_ext
(** An extended version of the regex (with the option and plus) *)

val empty : t_simp
(** The empty regex *)
val is_empty : t_simp -> bool
(** [is_empty r] checks if the regexp is the empty (not the empty word : ɛ ≠ ∅) *)

val letter : lt -> t_simp
(** [letter l] creates a regex with the letter [l] *)
val concat : t_simp -> t_simp -> t_simp
(** [concat r1 r2] creates a regex by concatenating [r1] with [r2] *)
val union : t_simp -> t_simp -> t_simp
(** [union r1 r2] creates a regex by uniting [r1] with [r2] *)
val star : t_simp -> t_simp
(** [star r] creates a regex with the Kleene star *)

val simp_to_ext : t_simp -> t_ext
(** [simp_to_ext r] extends the simple regex [r], in order to simplify or print it *)

val pp : t_ext -> string
(** [pp r] cast the regex [r] into a string *)

val simplify : t_ext -> t_ext
(** [simplify r] normalizes and simplfies the regex [r] as much as possible *)