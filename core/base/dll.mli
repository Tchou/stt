type 'a t
val empty : 'a t
val is_empty : 'a t -> bool
val push : 'a -> 'a t -> 'a t
val pop : 'a t -> 'a * 'a t
val cut_above : 'a t -> unit
val iter_above : ('a -> unit) -> 'a t -> unit
val iter_below : ('a -> unit) -> 'a t -> unit
val invalidate_above : ('a -> unit) -> 'a t -> unit