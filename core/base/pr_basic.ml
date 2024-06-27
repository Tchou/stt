open Format

type pr = formatter -> unit
type single =
  | Singleton of pr
  | Range of pr * pr

type 'a t = bool * ('a * single) list

let pr_string (s : string)
    (fmt : formatter) : unit =
  fprintf fmt "%s" s

let pp_single (fmt : formatter)
    (s : single) : unit =
  match s with
  | Singleton f -> fprintf fmt "%t" f
  | Range (f1, f2) -> fprintf fmt "%t--%t" f1 f2

let pp ?(pp_any : formatter -> unit = pr_string "Any")
    ?(pp_empty : formatter -> unit = pr_string "Empty")
    (fmt : formatter)
    (t : 'a t) : unit =
  match t with
  | false, [] -> pp_empty fmt
  | true, [] -> pp_any fmt
  | b, l -> fprintf fmt "@[(%b, %a)@]" b (pp_print_list
                                            ~pp_sep:(fun ppf () -> fprintf ppf ",@ ")
                                            (fun fmt (_, s) -> fprintf fmt "@[%a@]" pp_single s)
                                         ) l