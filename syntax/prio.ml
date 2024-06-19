type t = int

let lowest : t = 0

let pr_printer : t = 10
let pr_pair : t = 10
let pr_apply : t = 10
let pr_neg : t = 9
let pr_cap : t = 8
let pr_diff : t = 8
let pr_cup : t = 5
let pr_arrow : t = 4
let pr_rec : t = 3

let re_star : t = 7
let re_plus : t = 7
let re_option : t = 7 
let re_concat : t = 6
let re_union : t = 5

let compare = Int.compare