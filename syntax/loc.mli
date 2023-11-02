module Pos : Stt.Base.Common.T with type t = Lexing.position
include Stt.Base.Common.T with type t = Pos.t * Pos.t

val dummy : t

type 'a located = { descr : 'a; loc : t }

val with_loc : t -> 'a -> 'a located
val copy_loc : 'a located -> 'a -> 'a located