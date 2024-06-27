open Test_utils
open Parsing
open Stt
open Format

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
  let subst, s = Var.Set.fold (
    fun v (acc_l, acc_s) ->
      let n_var, acc_s =
        match List.assoc_opt v.Var.name var_map with
        | Some n_var ->
          n_var, asprintf "%s %a->%a" acc_s Var.dump v Var.dump n_var
        | None -> v, asprintf "%s %a" acc_s Var.dump v
      in
      Subst.add v (Typ.var n_var) acc_l, acc_s
  ) v' (Subst.of_list [], "")
  in
  Subst.apply subst t', s

let () =
  let check (s : string) : unit -> test_result =
    let s_pr =
      fun (fmt : formatter)
          (_ : unit) : unit ->
        fprintf fmt "%s" s
    in
    try
      let t = parse_type @@ "type t ('a, 'b, 'c, 'd) = " ^ s in
      let pp = asprintf "%a" Syntax.Pretty.pp t in
      let pp_pr =
        fun (fmt : formatter)
            (_ : unit) : unit ->
          fprintf fmt "%s" pp
      in
      try
        let t', _var_s = alpha_renaming t
          @@ parse_type
          @@ sprintf "type PP_TYPE ('a, 'b, 'c, 'd) = %s"
          @@ pp
        in
        fun (_ : unit) : test_result ->
          if Typ.equiv t t' then
            Ok (s_pr, pp_pr)
          else
            Error (s_pr, pp_pr)
      with e ->
        fun (_ : unit) : test_result ->
          let err =
            fun (fmt : formatter)
                (_ : unit) : unit ->
              fprintf fmt
                "REPARSING_ERROR : %s \n @[@[%a@] \n @[VS@] \n\n @[%a@]]\n\n"
                (Printexc.to_string e) s_pr () pp_pr ()
          in
          Error (pp_pr, err)
    with e ->
      fun (_ : unit) : test_result ->
        let err =
          fun (fmt : formatter)
              (_ : unit) : unit ->
            fprintf fmt "PARSING_ERROR : %s\n\n" @@ Printexc.to_string e
        in
        Error (s_pr, err)
  in
  run ~id:"syntax" "reparsing" [
    "basic",
      [
        check "Any" ;
        check "Empty" ;
        check "Int" ;
        check "Bool" ;
        check "42" ;
        check "`true" ;
        check "'c'--'o'" ;
        check "*--*" ; (* = Int *)
        check "*---13 | 4--*" ;
        check "Int | Bool" ;
        check "Any \\ Bool" ;
        check "(Int | Bool) & ~Bool" ; (* = Int *)
        check "Bool \\ `true" ; (* = `false *)
        check "Bool -> Char | Unit -> Int" ;
        check "Bool -> Char -> Int -> (Int & Bool)"
      ] ;
    "where",
      [
        check "X where X = (Any, X) | `nil" ;
        check "X where X = (Int, X) | `nil" ;
        check "(X, X) where X = (Int, X) | `nil" ;
        check "X where X = (Y, X) | `nil and Y = (Int, Y) | `nil" ;

        (* ERROR : "PARSING_ERROR : File \"core/typ.ml\", line 244, characters 2-8: Assertion failed" *)
        check "(Int, X) | Bool
          where X = Y & Z
          and Y = X | Char
          and Z = (Bool, Z) | `nil" ;

        (* ERROR : "PARSING_ERROR : File \"core/typ.ml\", line 244, characters 2-8: Assertion failed" *)
        check "(1, X) | 2
          where X = Y & Z
          and Y = X | 3
          and Z = (4, Z) | `nil"
      ] ;
    "regexp",
      [
        check "[ Int ]" ;
        check "[ Any* ]" ;
        check "[ Int* ]" ;
        check "[ Int;Bool+;Int ]" ;

        (* Interesting pp.... [(Int | Bool | Char+)?] *)
        check "[ Int | Bool | Char* ]" ;

        (* Interesting pp.... [( [Int*]; [Int*]* )?] *)
        check "[ [ Int* ]* ]" ;
        check "[
          Int;Int;Int
          | Int;Bool;Int;Int;Int
          | Int;Bool;Int;Bool*;Int;Int
        ]" (* -> regexp factorization *)
      ] ;
    "types",
      [
        check "(Int, X) where X = [ Int; X ] | `nil" ;
        check "[ Int; X ] where X = (Int, X) | `nil" ;
        check "[ 42* ; (X | Y?)+ ] where X = (Any, X) | `nil and Y = (Empty, Y) | `nil" ;

        (* ERROR : "PARSING_ERROR : File \"core/typ.ml\", line 244, characters 2-8: Assertion failed" *)
        check "X where X = [ Y ] | `nil
          and Y = X" ;

        (* ERROR : "PARSING_ERROR : File \"core/typ.ml\", line 244, characters 2-8: Assertion failed" *)
        check "(X, X)
          where X = (X, Y) | `nil
          and Y = Y | (Y, Z) | `nil
          and Z = Int | X"
      ] ;

    "basic_w_var",
      [
        check "'a" ;
        check "Any & 'a" ;
        check "Empty | 'a" ;
        check "(Int, 'a | 'b)";
        check "Bool & 'a" ;
        check "Any \\ 'a" ;
        check "('a | 'b) & ~'b" ; (* = 'a *)

        (* FAILED *)
        check "'a -> 'b -> 'c -> 'd" ;

        (* FAILED *)
        check "'a -> Unit"
      ] ;
    "where_w_var",
      [
        check "X where X = ('a, X) | `nil" ;
        check "X where X = ('a, X) | ('b, X) | `nil" ;
        check "(X, X) where X = ('a, X) | `nil" ;
        check "X where X = (Y, X) | `nil and Y = ('a, Y) | `nil" ;

        (* ERROR : "PARSING_ERROR : File \"core/typ.ml\", line 244, characters 2-8: Assertion failed" *)
        check "('a, X) | Bool
          where X = Y & Z
          and Y = X | 'b
          and Z = ('c, Z) | `nil" ;
      ] ;
    "regexp_w_var",
      [
        check "[ 'a ]" ;
        check "[ 'a* ]" ;

        (* ERROR : "REPARSING_ERROR : Failure(\"Cannot mix cap and regular expression\")" *)
        check "[ 'a; ('b)+; 'a ]" ;

        check "[ 'a | 'b | 'c* ]" ;
        check "[ [ 'a* ]* ]" ;

        (* ERROR : "REPARSING_ERROR : Failure(\"Cannot mix cap and regular expression\")" *)
        check "[
          'a;'a;'a
          | 'a;'b;'a;'a;'a
          | 'a;'b;'a;('b)*;'a;'a
        ]" (* -> regexp factorization *)
      ] ;
    "types_w_var",
      [
        check "('a, X) where X = [ 'a; X ] | `nil" ;
        check "[ 'a; X ] where X = ('a, X) | `nil" ;

        (* ERROR : "REPARSING_ERROR : Failure(\"Cannot mix cap and regular expression\")" *)
        check "[ ('a)* ; (X | Y?)+ ]
          where X = ('b, X) | `nil
          and Y = ('c, Y) | `nil" ;

        (* ERROR : "PARSING_ERROR : File \"core/typ.ml\", line 244, characters 2-8: Assertion failed" *)
        check "X where X = [ X* ; X* ; ('a, 'a) ] | ('b, `nil)" ;

        (* ERROR : "PARSING_ERROR : File \"core/typ.ml\", line 244, characters 2-8: Assertion failed" *)
        check "(X, X)
          where X = (X, Y) | `nil
          and Y = Y | (Y, Z) | `nil
          and Z = 'a | X"
      ] ;
  ]