module Make (V : Common.T) =
struct
  type v = V.t
  type t = { id : int; data : v}

  module H = Hashtbl.Make(V)
  let h = H.create 16

  let equal h1 h2 = h1 == h2
  let compare h1 h2 =
    if equal h1 h2 then 0 else V.compare h1.data h2.data
  let hash h = h.id

  let pp fmt n = Format.fprintf fmt "@[%d:%a@]" n.id V.pp n.data
  let uid = ref ~-1
  let make data =
    try H.find h data with Not_found ->
      let id = incr uid; !uid in
      let n = { id; data} in
      H.add h data n; n

  let (!!) n = n.data
  let id n = n.id
end