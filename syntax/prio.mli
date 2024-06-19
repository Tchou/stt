type t = private int

val lowest : t

val pr_printer : t
val pr_pair : t
val pr_apply : t
val pr_neg : t
val pr_cap : t
val pr_diff : t
val pr_cup : t
val pr_arrow : t
val pr_rec : t

val re_concat : t
val re_union : t
val re_star : t
val re_plus : t
val re_option : t

val compare : t -> t -> int