type  ('te, 'ident) decl =
  { name :  'ident;
    params : 'ident list;
    expr : 'te }

module Open = struct
  type ('te, 'ident, 're, 'extra) typ = [
    (* Basic types *)
    | `Typ of Stt.Typ.t
    (* Constructors *)
    | `Pair of 'te * 'te
    | `Arrow of 'te * 'te
    (* Connectives *)
    | `Cup of 'te list
    | `Cap of 'te list
    | `Diff of 'te * 'te
    | `Neg of 'te
    (* Polymorphic variable *)
    | `Var of 'ident
    (* Regexp *)
    | `Regexp of 're
    (* Type nodes (instantiation and recursive types) *)
    | `Node of ('te, 'ident) node ref
    (* Extensions that are not used by the concrete syntax *)
    | `Extra of 'extra
  ]
  and ('te, 'ident) node = [
      `Inst of 'ident * 'te list
    | `Rec of 'te * ('ident * 'te) list
    | `From of 'ident *  ('ident * 'te list)
    | `Expr of 'te
  ]
  and ('te, 're) re = [
      `Re_epsilon
    | `Re_typ of 'te
    | `Re_star of 're
    | `Re_alt of 're * 're
    | `Re_concat of 're * 're
  ]



end


module Located =
struct
  type ident = Ident.t Loc.located
  type typ = (typ, ident, re, unit) Open.typ Loc.located
  and node = (typ, ident) Open.node
  and re = (typ, re) Open.re Loc.located
  type nonrec decl = (typ, ident) decl
end

module Simple =
struct
  type ident = Ident.t
  type 'a typ = ('a typ, ident, 'a re, 'a) Open.typ
  and 'a node = ('a typ, ident) Open.node
  and 'a re = ('a typ, 'a re) Open.re
  type nonrec 'a decl = ('a typ, ident) decl
end