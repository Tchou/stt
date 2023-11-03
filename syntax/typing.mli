exception Type_error of string
val error : ?loc:Loc.t -> ('a, Format.formatter, unit, 'b) format4 -> 'a

module Name = Stt.Base.Hstring


module Env :
sig
  type 'a t
  val empty : 'a t
  val add : Ast.lident -> 'a -> 'a t -> 'a t
  val replace : Ast.lident -> 'a -> 'a t -> 'a t
  val find : Ast.lident -> 'a t -> 'a
  val find_loc : Ast.lident -> 'a t -> 'a * Loc.t
  val find_unloc : Name.t -> 'a t -> 'a * Loc.t
  val find_opt : Ast.lident -> 'a t -> 'a option
  val find_loc_opt : Ast.lident -> 'a t -> ('a * Loc.t) option
  val find_unloc_opt : Name.t -> 'a t -> ('a * Loc.t) option
  val mem : Ast.lident -> 'a t -> bool
  val mem_unloc : Name.t -> 'a t -> bool
end
type global_decl = {
  decl : Ast.typ_decl;
  vars : (Name.t * Stt.Var.t) list;
  typ : Stt.Typ.t;
  recs : Ast.typ_expr Env.t;
}
type global = global_decl Env.t

val empty : global
val default : global

val type_decl :
  global_decl Env.t -> Ast.typ_decl -> global_decl * global_decl Env.t
