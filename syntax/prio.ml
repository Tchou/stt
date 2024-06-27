type t = int

let lowest : t = 0

let pr_printer : t = 8
let pr_pair : t = 8
let pr_apply : t = 8
let pr_neg : t = 7
let pr_cap : t = 6
let pr_diff : t = 6
let pr_cup : t = 3
let pr_arrow : t = 2
let pr_rec : t = 1

let re_star : t = 5
let re_plus : t = 5
let re_option : t = 5
let re_concat : t = 4
let re_union : t = 3

let compare = Int.compare