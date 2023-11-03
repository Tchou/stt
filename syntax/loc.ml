module Pos =
struct
  type t = Lexing.position

  let name = "Pos"
  let compare p1 p2 =
    let open Stt.Base.Common.Let in
    let open Lexing in
    let open Stdlib.Int in
    let<> () = String.compare p1.pos_fname p2.pos_fname in
    let<> () = compare p1.pos_lnum p2.pos_lnum in
    let<> () = compare p1.pos_bol p2.pos_bol in
    let<> () = compare p1.pos_cnum p2.pos_cnum in
    0

  let equal p1 p2 = compare p1 p2 = 0
  let hash p = Hashtbl.hash p

  let pp fmt p =
    Format.fprintf fmt "line %d, character %d"
      p.Lexing.pos_lnum
      (p.Lexing.pos_cnum -p.Lexing.pos_bol)

end

module L = struct
  include Stt.Base.Common.Pair(Pos)(Pos)
  let pp fmt (p1, p2) =
    Format.fprintf fmt "from %a to %a" Pos.pp p1 Pos.pp p2
end
include L
let dummy : t = Lexing.dummy_pos,Lexing.dummy_pos

type 'a located = { descr : 'a; loc : t }

let with_loc loc descr = {descr; loc}
let copy_loc d descr = { d with descr }
