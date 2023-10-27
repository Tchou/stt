open Stt.Base

type ('info, 'a) node = { info : 'info; descr : 'a }
type 'info lident = ('info, Hstring.t) node

type 'info typ_expr_ =
  (* Basic types *)
  | Typ of Stt.Typ.t
  (* Constructors *)
  | Pair of 'info typ_expr * 'info typ_expr
  | Arrow of 'info typ_expr * 'info typ_expr
  (* Connectives *)
  | Cup of 'info typ_expr * 'info typ_expr
  | Cap of 'info typ_expr * 'info typ_expr
  | Diff of 'info typ_expr * 'info typ_expr
  | Neg of 'info typ_expr
  (* Polymorphism *)
  | Var of Hstring.t
  | Inst of 'info lident * 'info typ_expr list
  (* Syntactic sugar *)
  | Regexp of 'info re

and 'info typ_expr = ('info, 'info typ_expr_) node

and 'info re_ =
    Re_epsilon
  | Re_typ of 'info typ_expr
  | Re_star of 'info re
  | Re_alt of 'info re * 'info re
  | Re_concat of 'info re * 'info re

and 'info re = ('info, 'info re_) node

type 'info typ_decl = { name : 'info lident;
                        params : 'info lident list;
                        expr : 'info typ_expr }
