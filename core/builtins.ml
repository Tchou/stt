module HS = Base.Hstring
open Typ

let by_names_ref = ref []
let register n (t:t) =
  by_names_ref := (HS.cons n, t) :: !by_names_ref; t

let any = register "Any" any
let empty = register "Empty" empty
let nil = register "Nil" @@ Singleton.enum "nil"

let unit = register "Unit" @@ Singleton.unit

let true_ = register "True" @@ Singleton.enum "true"
let false_ = register "False" @@ Singleton.enum "false"
let bool = register "Bool" @@ cup true_ false_

let int = register "Int" @@ VarInt.set VarInt.any empty
let char = register "Char" @@ VarChar.set VarChar.any empty
let enum = register "Enum" @@ VarEnum.set VarEnum.any empty

let arrow = register "Arrow" @@ arrow (node empty) (node any)

let z_interval i j =
  VarInt.set (VarInt.leaf (Int.range i j)) empty

let s_interval s =
  let s = s - 1 in
  let n = Z.(one lsl  s) in
  z_interval Z.(~-n) Z.(pred n)

let u_interval u =
  let n = Z.(one lsl  u) in
  z_interval Z.zero n

let int8 = register "Int8" @@ s_interval 8
let uint8 = register "Uint8" @@ u_interval 8
let int16 = register "Int16" @@ s_interval 16
let uint16 = register "Uint16" @@ u_interval 16
let int32 = register "Int32" @@ s_interval 32
let uint32 = register "Uint32" @@ u_interval 32
let int64 = register "Int64" @@ s_interval 64
let uint64 = register "Uint64" @@ u_interval 64

let posint = register "PosInt" @@ VarInt.set (VarInt.leaf (Int.right Z.zero)) empty
let negint = register "NegInt" @@ VarInt.set (VarInt.leaf (Int.left Z.zero)) empty













let by_names = !by_names_ref