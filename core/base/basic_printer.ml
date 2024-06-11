type t =
  | Item of (Format.formatter -> unit)
  | Neg of t
  | Cap of t list
  | Cup of t list
  | Dif of t list

let _simp (bp : t) : t =
  let rec flatten_aux (f : t -> t list option)
                      (l : t list) : t list =
    let flatten_aux' = flatten_aux f in           
    match l with
    | [] -> []
    | r :: l -> (
      match f r with
      | Some ll -> List.append ll @@ flatten_aux' l
      | None -> r :: flatten_aux' l
    )
  and flatten (bp : t) : t =
    match bp with
    | Cap l ->
      let f = 
        fun (bp : t) : t list option ->
          match bp with
          | Cap l -> Some l
          | _ -> None
      in
      Cap (flatten_aux f @@ List.map flatten l)
    | Cup l ->
      let f = 
        fun (bp : t) : t list option ->
          match bp with
          | Cup l -> Some l
          | _ -> None
      in
      Cup (flatten_aux f @@ List.map flatten l)
    | _ -> bp
  in
  flatten bp