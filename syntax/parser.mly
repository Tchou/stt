%{
open Loc

let parse_error _pos fmt = (* Todo change *)
  Format.kasprintf (fun s -> failwith s) fmt

let typ_nil = `Typ Stt.Builtins.nil
let typ_unit = `Typ Stt.Builtins.unit

(* Construct built-in types as much as possible *)
let from_typ ftt fother (t1:Ast.Located.typ) (t2:Ast.Located.typ) =
    match t1.descr, t2.descr with
        | `Typ s1, `Typ s2 -> `Typ (ftt s1 s2)
        | _ -> fother t1 t2

let cup = from_typ Stt.Typ.cup (fun t1 t2 -> `Cup [t1; t2])
let cap = from_typ Stt.Typ.cap (fun t1 t2 -> `Cap [t1; t2])
let diff = from_typ Stt.Typ.diff (fun t1 t2 -> `Diff (t1, t2))

let pair = from_typ (fun s1 s2 -> Stt.Typ.(product (node s1) (node s2)))
                       (fun t1 t2 -> `Pair (t1, t2))
let arrow = from_typ (fun s1 s2 -> Stt.Typ.(arrow (node s1) (node s2)))
                       (fun t1 t2 -> `Arrow (t1, t2))
let neg = function
    | {descr = `Typ d; _} -> `Typ (Stt.Typ.neg d)
    | t -> `Neg t

(* Construct built-in types in regexps as much as possible *)

let cup_re sloc (r1:Ast.Located.re) (r2:Ast.Located.re) =
    match r1.descr, r2.descr with
        `Re_typ t1, `Re_typ t2 -> `Re_typ (Loc.with_loc sloc (cup t1 t2))
        | _ -> `Re_alt (r1, r2)

let from_re name cons sloc (r1:Ast.Located.re) (r2:Ast.Located.re) =
    match r1.descr, r2.descr with
        `Re_typ t1, `Re_typ t2 -> `Re_typ (Loc.with_loc sloc (cons t1 t2))
        | _ -> parse_error sloc "Cannot mix %s and regular expression" name

let arrow_re = from_re "arrow" arrow
let cap_re = from_re "cap" cap
let diff_re = from_re "diff" diff
let prod_re = from_re "product" pair


%}

%start <Ast.Located.decl> typ_decl
%start <Ast.Located.typ> typ_expr

/* These and other type annotations in the file help debugging errors with
   Polymorphic variants */
%type <Ast.Located.typ> typ
%type <Ast.Located.re> re

%%

/* Macros */

%inline located (expr): e = expr { Loc.with_loc $sloc e  }
;
%inline lident: l = located(IDENT) { l }
;
%inline eoi:
   EOF | EOP {()}
;
%inline sol:
    EOP* {()}
;

/* Entry points */

typ_decl:
sol; t = typ_decl_elem; eoi { t }
;

typ_expr:
sol; t = arrow_typ; eoi { t }
;

/* Declarations */

typ_decl_elem:
    "type"; name = lident;
    params = inst_params (located(VAR));
    "="; expr = typ {
        Ast.{ name; params; expr }
    }
;

typ: t = typ_ { Loc.with_loc $sloc t }

typ_:
  a = arrow_typ; "where"; l = and_ident_typ { `Node (ref (`Rec (a, List.rev l))) }
| a = arrow_typ { a.descr }
;

and_ident_typ:
    it = ident_typ   { [ it ]}
| l = and_ident_typ; "and" it = ident_typ { it :: l }
;
ident_typ:
x = lident; "="; t = typ { (x, t) }
;


arrow_typ: e = located(arrow_typ_) { e }

arrow_typ_:
|   t1 = or_typ; "->"; t2 = arrow_typ { arrow t1 t2 }
|   t = or_typ        {  t.descr }
;


or_typ: t = located(or_typ_) { t }

or_typ_:
|   t1 = or_typ; "|"; t2 = and_typ { cup t1 t2 }
|   t = and_typ { t.descr }
;


and_typ: t = located(and_typ_) { t }

and_typ_:
|   t1 = and_typ ; "&"; t2 = prod_paren_typ { cap t1 t2 }
|   t1 = and_typ ; "\\"; t2 = prod_paren_typ { diff t1 t2 }
|   t = prod_paren_typ { t.descr }
;


prod_paren_typ: t = located(prod_paren_typ_) { t }

prod_paren_typ_:
|   "("; o = option(pair(typ, option(preceded(",", typ)))); ")" {
    match o with
        None -> typ_unit
        | Some (t1, None) -> t1.descr
        | Some (t1, Some t2) -> pair t1 t2
    }
| t = simple_typ { t.descr }
;


simple_typ: t = located(simple_typ_) { t }

simple_typ_:
|   v = located(VAR)  { `Var v }

|   z = INT { `Typ (Stt.Typ.Singleton.int z)   }
|   i1=int_or_star; "--"; i2=int_or_star {
    let open Stt in
    let t =
        match i1, i2 with
            None, None -> Typ.(VarInt.set VarInt.any empty)
            | Some i1, None -> Typ.(VarInt.set (VarInt.leaf (Int.right i1)) empty)
            | None, Some i2 -> Typ.(VarInt.set (VarInt.leaf (Int.left i2)) empty)
            | Some i1, Some i2 -> Typ.(VarInt.set (VarInt.leaf (Int.range i1 i2)) empty)
    in `Typ t
}

|   c = CHAR { `Typ (Stt.Typ.Singleton.char c)   }
|   c1 = CHAR; "--"; c2 = CHAR {
    `Typ Stt.Typ.(VarChar.set (VarChar.leaf (Stt.Char.range c1 c2)) empty)}


|   a = ENUM { `Typ (Stt.Typ.(VarEnum.set (VarEnum.leaf (Stt.Enum.singleton a)) empty))   }
|   s = STRING {
        let open Stt.Typ in
        let t = ref (Stt.Builtins.nil) in
        for i = Array.length s - 1 downto 0 do
            t:=  product ( node (Singleton.char s.(i))) (node !t);
        done;
        `Typ !t
    }

|   x = lident;
    ofrom = option (preceded("from", lident));
    args = inst_params(typ) {
        `Node (ref (
            match ofrom with
            None -> `Inst (x, args)
            | Some y -> `From (x, (y, args))
        ))
    }

|   "~"; e = prod_paren_typ { neg e }
|   "["; ore = re ? ; "]"  { match ore with
                              None -> typ_nil
                            | Some re -> `Regexp re }
;
%inline inst_params (X):
              { [] }
| l = delimited("(",separated_nonempty_list(",", X),")") { l }
;
%inline int_or_star:
    i = INT { Some i}
    | "*"   { None  }


re: re = located(re_)  { re }

re_:
|   re1 = or_re; "->"; re2 = re {
        arrow_re $sloc re1 re2
    }
| re = or_re { re.descr }
;

or_re: re = located (or_re_)  { re }

or_re_:
|  re1 = concat_re; "|"; re2 = or_re {
    cup_re $sloc re1 re2
    }
|  re = concat_re { re.descr }
;

concat_re: re = located (concat_re_) { re }

concat_re_:
|  re1 = concat_re; ";" re2 = and_re { `Re_concat (re1, re2) }
|  re = and_re { re.descr }
;

and_re: re = located(and_re_) { re }

and_re_:
|  re1 = and_re; "&"; re2 = simple_re {
    cap_re $sloc re1 re2
    }
|  re1 = and_re; "\\"; re2 = simple_re {
    diff_re $sloc re1 re2
    }
| re = simple_re { re.descr }
;

simple_re: re = located(simple_re_) { re }

simple_re_:
|   typ = simple_typ               { `Re_typ typ }
|   re = simple_re; "*"            { `Re_star re }
|   re = simple_re; "+"            { `Re_concat (re, (Loc.copy_loc re (`Re_star re))) }
|   re = simple_re; "?"            { `Re_alt ((Loc.copy_loc re `Re_epsilon), re) }
|   "("; o = option (pair(re, option(preceded (",", re )))); ")"  {
        match o with
          None -> `Re_typ (Loc.with_loc $sloc typ_unit)
        | Some (re, None) -> re.descr
        | Some (re1, Some re2) -> prod_re $sloc re1 re2
}
;
