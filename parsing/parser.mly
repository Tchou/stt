%token <Z.t> INT
%token <Uchar.t> CHAR
%token <Stt.Base.Hstring.t> ATOM
%token <Stt.Base.Hstring.t> NAME
%token <Stt.Base.Hstring.t> VAR
%token MINUSMINUS "--"
%token MINUSGT "->"
%token OR "|"
%token AND "&"
%token DIFF "\\"
%token NOT "~"
%token STAR "*"
%token PLUS "+"
%token QMARK "?"
%token COMMA ","
%token SEMI ";"
%token LP "("
%token RP ")"
%token LSB "["
%token RSB "]"
%token EOF

%{
open Ast
let mk_loc sloc descr =
    { info = sloc; descr }

let copy_loc e descr =
    { e with descr }
let parse_error msg = failwith msg (* todo *)

let typ_nil = Typ Stt.(Typ.Singleton.atom (Base.Hstring.make "nil"))
let typ_unit = Typ Stt.Typ.Singleton.unit


let cup_re sloc r1 r2 =
    match r1.descr, r2.descr with
        Re_typ t1, Re_typ t2 -> Re_typ (mk_loc sloc (Cup (t1, t2)))
        | _ -> Re_alt (r1, r2)

let from_re name cons sloc r1 r2 =
    match r1.descr, r2.descr with
        Re_typ t1, Re_typ t2 -> Re_typ (mk_loc sloc (cons t1 t2))
        | _ -> parse_error ("Cannot mix " ^ name ^ " and regular expression")

let arrow_re = from_re "arrow" (fun t1 t2 -> Arrow (t1, t2))
let cap_re = from_re "cap" (fun t1 t2 -> Cap (t1, t2))
let diff_re = from_re "diff" (fun t1 t2 -> Diff (t1, t2))
let prod_re = from_re "product" (fun t1 t2 -> Pair (t1, t2))

%}

%start < _ typ_expr> typ

%%

%inline located (expr):
e = expr { mk_loc $sloc e  }


typ:
    e = arrow_typ EOF   { e }

arrow_typ:
    e = located(arrow_typ_) { e }


arrow_typ_:
|   t1 = or_typ; "->"; t2 = arrow_typ { Arrow (t1, t2) }
|   t = or_typ        {  t.descr }

or_typ:
    t = located(or_typ_) { t }

or_typ_:
|   t1 = or_typ; "|"; t2 = and_typ { Cup (t1, t2) }
|   t = and_typ { t.descr }

and_typ:
    t = located(and_typ_) { t }

and_typ_:
    t1 = and_typ ; "&"; t2 = prod_paren_typ { Cap (t1, t2 )}
|   t1 = and_typ ; "\\"; t2 = prod_paren_typ { Diff (t1, t2 )}
|   t = prod_paren_typ { t.descr }

prod_paren_typ:
    t = located(prod_paren_typ_) { t }

prod_paren_typ_:
   "("; o = option(pair(arrow_typ, option(preceded(",", arrow_typ)))); ")" {
    match o with
        None -> typ_unit
        | Some (t1, None) -> t1.descr
        | Some (t1, Some t2) -> Pair(t1, t2)
    }
| t = simple_typ { t.descr }

simple_typ:
    t = located(simple_typ_) { t }

simple_typ_:
|   v = VAR  { Var v }

|   z = INT { Typ (Stt.Typ.Singleton.int z)   }
|   i1=int_or_star; "--"; i2=int_or_star { 
    let open Stt in
    let t =
        match i1, i2 with
            None, None -> Typ.(Set.int empty VarInt.any)
            | Some i1, None -> Typ.(Set.int empty (VarInt.leaf (Int.right i1)))
            | None, Some i2 -> Typ.(Set.int empty (VarInt.leaf (Int.left i2)))
            | Some i1, Some i2 -> Typ.(Set.int empty (VarInt.leaf (Int.range i1 i2)))
    in Typ t
}

|   c = CHAR { Typ (Stt.Typ.Singleton.char c)   }
|   c1 = CHAR; "--"; c2 = CHAR {
    Typ Stt.Typ.(Set.char empty (VarChar.leaf (Stt.Char.range c1 c2)))}


|   a = ATOM { Typ (Stt.Typ.Singleton.atom a)   }

| x = located(NAME) ; o = option (delimited("(",separated_nonempty_list(",", arrow_typ),")")) {
            Inst (x, match o with None -> [] | Some l -> l)
    }

|   "~"; e = prod_paren_typ { Neg e }
|   "["; ore = re ? ; "]"  { match ore with
                              None -> typ_nil
                            | Some re -> Regexp re }

%inline int_or_star:
    i = INT { Some i}
    | "*"   { None  }

re:
    re = located(re_)  { re }

re_:
re1 = or_re; "->"; re2 = re {
    arrow_re $sloc re1 re2
}
| re = or_re { re.descr }

or_re:
    re = located (or_re_)  { re }

or_re_:
re1 = concat_re; "|"; re2 = or_re {
    cup_re $sloc re1 re2
}
| re = concat_re { re.descr }

concat_re:
    re = located (concat_re_) { re }

concat_re_:
re1 = concat_re; ";" re2 = and_re { Re_concat (re1, re2) }
| re = and_re { re.descr }

and_re:
    re = located(and_re_) { re }

and_re_:
re1 = and_re; "&"; re2 = simple_re {
    cap_re $sloc re1 re2
}
| re1 = and_re; "\\"; re2 = simple_re {
    diff_re $sloc re1 re2
}
| re = simple_re { re.descr }

simple_re:
    re = located(simple_re_) { re }

simple_re_:
    typ = simple_typ               { Re_typ typ }
|   re = simple_re; "*"            { Re_star re }
|   re = simple_re; "+"            { Re_concat (re, (copy_loc re (Re_star re))) }
|   re = simple_re; "?"            { Re_alt ((copy_loc re Re_epsilon), re) }
|   "("; o = option (pair(re, option(preceded (",", re )))); ")"  {
        match o with
          None -> Re_typ (mk_loc $sloc typ_unit)
        | Some (re, None) -> re.descr
        | Some (re1, Some re2) -> prod_re $sloc re1 re2
}
