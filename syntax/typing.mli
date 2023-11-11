exception Type_error of string
val error : ?loc:Loc.t -> ('a, Format.formatter, unit, 'b) format4 -> 'a

module Env :
sig
  type 'a t
  val empty : 'a t
  val add : Ast.Located.ident -> 'a -> 'a t -> 'a t
  val replace : Ast.Located.ident -> 'a -> 'a t -> 'a t
  val find : Ast.Located.ident -> 'a t -> 'a
  val find_loc : Ast.Located.ident -> 'a t -> 'a * Loc.t
  val find_unloc : Ident.t -> 'a t -> 'a * Loc.t
  val find_opt : Ast.Located.ident -> 'a t -> 'a option
  val find_loc_opt : Ast.Located.ident -> 'a t -> ('a * Loc.t) option
  val find_unloc_opt : Ident.t -> 'a t -> ('a * Loc.t) option
  val mem : Ast.Located.ident -> 'a t -> bool
  val mem_unloc : Ident.t -> 'a t -> bool
  val to_seq : 'a t -> (Ident.t * ('a * Loc.t)) Seq.t
end

type global_decl = {
  decl : Ast.Located.decl;
  vars : (Ident.t * Stt.Var.t) list;
  typ : Stt.Typ.t;
  recs : Ast.Located.typ Env.t;
}

module GlobalDecl : Stt.Base.Common.T with type t = global_decl

type global = GlobalDecl.t Env.t

val empty : global
val default : global

val type_decl : global -> Ast.Located.decl -> GlobalDecl.t * global
