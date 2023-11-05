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
  val to_seq : 'a t -> (Name.t * ('a * Loc.t)) Seq.t
end

type global_decl = {
  decl : Ast.typ_decl;
  vars : (Name.t * Stt.Var.t) list;
  typ : Stt.Typ.t;
  recs : Ast.typ_expr Env.t;
}

module GlobalDecl : Stt.Base.Common.T with type t = global_decl

type global = GlobalDecl.t Env.t

val empty : global
val default : global

val type_decl : global -> Ast.typ_decl -> GlobalDecl.t * global
