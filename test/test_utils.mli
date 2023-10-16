module type TESTABLE = sig
  type t

  val pp : Format.formatter -> t -> unit
  val equal : t -> t -> bool
end

type 'a testable = (module TESTABLE with type t = 'a)

module T : sig
  val string : (module TESTABLE with type t = string)
  val int : (module TESTABLE with type t = int)
  val bool : (module TESTABLE with type t = bool)
end

type printer = Format.formatter -> unit -> unit
type test_result = (printer * printer, printer * printer) result

val check : 't testable -> 't -> 't -> unit -> test_result
val summary : string -> unit

val run :
  ?id:string -> string -> (string * (unit -> test_result) list) list -> unit
