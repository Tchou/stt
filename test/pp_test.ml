open Utils
open Parsing
open Stt

let alpha_renaming (t : Typ.t)
                   (t' : Typ.t) =
  let vp, vn = Typ.vars t in
  let v = Var.Set.union vp vn in
  let var_map = Var.Set.fold (
    fun v acc -> (v.Var.name, v) :: acc
  ) v []
  in
  let vp', vn' = Typ.vars t' in
  let v' = Var.Set.union vp' vn' in
  let subst = Var.Set.fold (
    fun v acc ->
      let n_var =
        match List.assoc_opt v.Var.name var_map with
        | Some n_var -> n_var
        | None -> v
      in
      Subst.add v (Typ.var n_var) acc
  ) v' @@ Subst.of_list []
  in
  Subst.apply subst t'

let () =
  let check (s : string) : unit -> test_result =
    try
      let t = parse_type s in 
      let pp = Format.asprintf "%a" Syntax.Pretty.pp t in
      try
        let t' = alpha_renaming t
          @@ parse_type 
          @@ Format.sprintf "type PP_TYPE ('a, 'b, 'c, 'd) = %s"
          @@ pp
        in
        check T.bool true @@ Typ.equiv t t'
      with e ->
        fun (_ : unit) : test_result -> 
          let msg = (^) "REPARSING_ERROR : " @@ Printexc.to_string e in
          raise @@ Except msg
        (* fun (_ : unit) : test_result ->
          let s =
            fun (fmt : Format.formatter)
                (_ : unit) : unit ->
              Format.fprintf fmt "%s" s
          in
          let pp =
            fun (fmt : Format.formatter)
                (_ : unit) : unit ->
              Format.fprintf fmt "%s" pp
          in
          Error (s, pp) *)
    with e ->
      fun (_ : unit) : test_result -> 
        let msg = (^) "PARSING_ERROR : " @@ Printexc.to_string e in
        raise @@ Except msg
  in
  run ~id:"syntax" "Pp parsed" [
    "basic", 
      [
        check "type t = Any" ;
        check "type t = Empty" ;
        check "type t = Int" ;
        check "type t = Bool" ;
        check "type t = 42" ;
        check "type t = `true" ;
        check "type t = 'c' -- 'o'" ;
        check "type t = * -- *" ; (* = Int *)

        (* ERROR : "REPARSING_ERROR : Failure(\"Unexpected character ''\")" *)
        check "type t = * -- -13 | 4 -- *" ;

        check "type t = Int | Bool" ;
        check "type t = Any \\ Bool" ;
        check "type t = (Int | Bool) & ~Bool" ; (* = Int *)
        check "type t = Bool \\ `true" ; (* = `false *)
        check "type t = Bool -> Char | Unit -> Int" ;
        check "type t = Bool -> Char -> Int -> (Int & Bool)"
      ] ;
    "where",
      [
        check "type t = X where X = (Any, X) | `nil" ;
        check "type t = X where X = (Int, X) | `nil" ;
        check "type t = (X, X) where X = (Int, X) | `nil" ;
        check "type t = X where X = (Y, X) | `nil and Y = (Int, Y) | `nil" ;
        
        (* ERROR : "PARSING_ERROR : File \"core/typ.ml\", line 244, characters 2-8: Assertion failed" *)
        check "type t = (Int, X) | Bool 
          where X = Y & Z 
          and Y = X | Char
          and Z = (Bool, Z) | `nil" ;
        
        (* ERROR : "PARSING_ERROR : File \"core/typ.ml\", line 244, characters 2-8: Assertion failed" *)
        check "type t = (1, X) | 2 
          where X = Y & Z 
          and Y = X | 3
          and Z = (4, Z) | `nil"
      ] ;
    "regexp", 
      [
        check "type t = [ Int ]" ;
        check "type t = [ Any* ]" ;
        check "type t = [ Int* ]" ;
        check "type t = [ Int;Bool+;Int ]" ;
        check "type t = [ Int | Bool | Char* ]" ;
        check "type t = [ [ Int* ]* ]" ;
        check "type t = [ 
          Int;Int;Int 
          | Int;Bool;Int;Int;Int 
          | Int;Bool;Int;Bool*;Int;Int  
        ]" (* -> regexp factorization *) 
      ] ;
    "types", 
      [
        check "type t = (Int, X) where X = [ Int; X ]" ;
        check "type t = [ Int; X ] where X = (Int, X) | `nil" ;
        check "type t = [ 42* ; (X | Y?)+ ] where X = (Any, X) | `nil and Y = (Empty, Y) | `nil" ;

        (* ERROR : "PARSING_ERROR : File \"core/typ.ml\", line 244, characters 2-8: Assertion failed" *)

        check "type t = X where X = [ X* ; X* ; (Int, Int) ] | (Bool, `nil)" ;
        (* ERROR : "PARSING_ERROR : File \"core/typ.ml\", line 244, characters 2-8: Assertion failed" *)
        check "type t = (X, X) 
          where X = (X, Y) | `nil 
          and Y = Y | (Y, Z) | `nil
          and Z = Int | X"
      ] ;

    "basic_w_var", 
      [
        check "type t ('a) = 'a" ;
        check "type t ('a) = Any & 'a" ;
        check "type t ('a) = Empty | 'a" ;
        check "type t ('a, 'b) = (Int, 'a | 'b)";
        check "type t ('a) = Bool & 'a" ;
        check "type t ('a) = Any \\ 'a" ;
        check "type t ('a, 'b) = ('a | 'b) & ~'b" ; (* = 'a *)
        check "type t ('a, 'b, 'c, 'd) = 'a -> 'b -> 'c -> 'd" ;
        check "type t ('a) = 'a -> 'a -> Unit"
      ] ;
    "where_w_var",
      [
        check "type t ('a) = X where X = ('a, X) | `nil" ;
        check "type t ('a, 'b) = X where X = ('a, X) | ('b, X) | `nil" ;
        check "type t ('a) = (X, X) where X = ('a, X) | `nil" ;
        check "type t ('a) = X where X = (Y, X) | `nil and Y = ('a, Y) | `nil" ;
        
        (* ERROR : "PARSING_ERROR : File \"core/typ.ml\", line 244, characters 2-8: Assertion failed" *)
        check "type t ('a, 'b, 'c) = ('a, X) | Bool 
          where X = Y & Z 
          and Y = X | 'b
          and Z = ('c, Z) | `nil" ;
      ] ;
    "regexp_w_var", 
      [
        check "type t ('a) = [ 'a ]" ;
        check "type t ('a) = [ 'a* ]" ;
        check "type t ('a, 'b) = [ 'a;'b+;'a ]" ;

        (* ERROR : "REPARSING_ERROR : Failure(\"Cannot mix cap and regular expression\")" *)
        check "type t ('a, 'b, 'c) = [ 'a | 'b | 'c* ]" ;

        check "type t ('a) = [ [ 'a* ]* ]" ;

        (* ERROR : "REPARSING_ERROR : Failure(\"Cannot mix cap and regular expression\")" *)
        check "type t ('a, 'b) = [ 
          'a;'a;'a 
          | 'a;'b;'a;'a;'a 
          | 'a;'b;'a;'b*;'a;'a  
        ]" (* -> regexp factorization *) 
      ] ;
    "types_w_var", 
      [
        check "type t ('a) = ('a, X) where X = [ 'a; X ]" ;
        check "type t ('a) = [ 'a; X ] where X = ('a, X) | `nil" ;
        
        (* ERROR : "REPARSING_ERROR : Failure(\"Cannot mix cap and regular expression\")" *)
        check "type t ('a, 'b, 'c) = [ 'a* ; (X | Y?)+ ] where X = ('b, X) | `nil and Y = ('c, Y) | `nil" ;
        
        (* ERROR : "PARSING_ERROR : File \"core/typ.ml\", line 244, characters 2-8: Assertion failed" *)
        check "type t ('a, 'b) = X where X = [ X* ; X* ; ('a, 'a) ] | ('b, `nil)" ;
        
        (* ERROR : "PARSING_ERROR : File \"core/typ.ml\", line 244, characters 2-8: Assertion failed" *)
        check "type t ('a) = (X, X) 
          where X = (X, Y) | `nil 
          and Y = Y | (Y, Z) | `nil
          and Z = 'a | X"
      ] ;
  ]