type kind =
  [ `user
  | `generated
  ]

let name = "Var"
let uid =
  let i = ref ~-1 in
  fun () ->
    incr i;
    !i

module V = struct
  type t = {
    id : int;
    name : string;
    kind : kind;
  }

  let make ?(kind = `user) s = { id = uid (); name = s; kind }
  let equal v1 v2 = v1.id == v2.id
  let hash v = v.id
  let compare v1 v2 = Stdlib.Int.compare v1.id v2.id
  let pp fmt v = Format.fprintf fmt "@['%s@]" v.name
end

include V

let dump fmt v =
  Format.fprintf fmt "'@[%s(%d,%s)@]" v.name v.id
    (if v.kind = `user then "`user" else "`generated")

module Set = Set.Make (V)

let incr_var b =
  let rec loop i b =
    if Bytes.get b i == 'z' then begin
      if i > 0 then begin
        Bytes.set b i 'a';
        loop (i - 1) b
      end
      else
        let l = Bytes.length b in
        let nb = Bytes.create (l + 1) in
        begin
          Bytes.set nb 0 'a';
          Bytes.blit b 0 nb 1 l;
          nb
        end
    end
    else
      let open Stdlib in
      let c = Char.chr (1 + Char.code (Bytes.get b i)) in
      Bytes.set b i c;
      b
  in
  loop (Bytes.length b - 1) b

module StrSet = Stdlib.Set.Make (String)

let name_not_in vset =
  let b = Bytes.of_string "a" in
  let sset = Set.fold (fun v acc -> StrSet.add v.name acc) vset StrSet.empty in
  let rec loop b =
    if StrSet.mem (Bytes.unsafe_to_string b) sset then begin
      loop (incr_var b)
    end
    else Bytes.unsafe_to_string b
  in
  loop b
