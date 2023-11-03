open Stt.Base

type lident = Hstring.t Loc.located

type typ_expr_ =
  (* Basic types *)
  | Typ of Stt.Typ.t
  (* Constructors *)
  | Pair of typ_expr *  typ_expr
  | Arrow of typ_expr *  typ_expr
  (* Connectives *)
  | Cup of typ_expr *  typ_expr
  | Cap of typ_expr *  typ_expr
  | Diff of typ_expr *  typ_expr
  | Neg of typ_expr
  (* Polymorphic variable *)
  | Var of lident
  (* Regexp *)
  | Regexp of re
  (* Type nodes (instantiation and recursive types) *)
  | Node of node ref

and node =
    Inst of lident * typ_expr list
  | Rec of typ_expr * (lident * typ_expr) list
  | From of lident *  (lident * typ_expr list)
  | Expr of typ_expr

and  typ_expr = typ_expr_ Loc.located

and  re_ =
    Re_epsilon
  | Re_typ of typ_expr
  | Re_star of re
  | Re_alt of re *  re
  | Re_concat of re *  re

and  re = re_ Loc.located

type  typ_decl = { name :  lident;
                   params :  lident list;
                   expr :  typ_expr }