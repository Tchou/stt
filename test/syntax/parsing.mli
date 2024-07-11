exception Except of string

val parse_type : ?debug : bool -> string -> Stt.Typ .t

val alpha_renaming : Stt.Typ.t -> Stt.Typ.t -> Stt.Typ.t * string