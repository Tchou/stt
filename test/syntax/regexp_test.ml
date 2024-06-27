open Test_utils
open Parsing
open Stt
open Format

let () =
  let check ?(b : bool = true)
            (t1 : string)
            (t2 : string) : unit -> test_result =
    try
      let t = parse_type @@ "type t1 ('a, 'b, 'c, 'd) = " ^ t1 in
      try
        let t' = parse_type @@ "type t2 ('a, 'b, 'c, 'd) = " ^ t2 in
        fun (_ : unit) : test_result ->
          let b' =
            b = Typ.equiv t t'
          in
          let expect =
            fun (fmt : formatter)
                (_ : unit) : unit ->
              fprintf fmt "%s %s %s" t1 (if b then "=" else "!=") t2
          in
          let res =
            fun (fmt : formatter)
                (_ : unit) : unit ->
              pp_print_bool fmt b'
          in
          if b' then
            Ok (expect, res)
          else
            Error (expect, res)
      with e ->
        fun (_ : unit) : test_result ->
          let t2 =
            fun (fmt : formatter)
                (_ : unit) : unit ->
              fprintf fmt "%s" t2
          in
          let msg = asprintf
            "PARSING_ERROR (t2) : %s \n %a \n\n"
            (Printexc.to_string e) t2 ()
          in
          raise @@ Except msg
    with e ->
      fun (_ : unit) : test_result ->
        let t1 =
          fun (fmt : formatter)
              (_ : unit) : unit ->
            fprintf fmt "%s" t1
        in
        let msg = asprintf
          "PARSING_ERROR (t1) : %s \n %a \n\n"
          (Printexc.to_string e) t1 ()
        in
        raise @@ Except msg
  in
  let check_false = check ~b:false
  in
  run ~id:"syntax" "two_syntax" [
    "basic",
      [
        check "(Int, `nil)" "[ Int ]" ;
        check "(42, `nil)" "[ 42 ]" ;
        check "(Bool, (Int, `nil))" "[ Bool; Int ]" ;
        check "`nil" "[]" ;
        check "((Int, `nil), Bool)" "([ Int ], Bool)" ;
        check_false "((Int, `nil), Bool)" "[ Int; Bool ]" ;
        check_false "(Int, Int)" "[ Int; Int ]" ;
      ] ;
    "rec",
      [
        check "X where X = (Int, X) | `nil" "[ Int* ]" ;
        check "X where X = (Int, (Int, X)) | `nil" "[ Int+ ]" ;
        check_false "X where X = (Int, X) | `nil" "[ [ Int* ]* ]" ;
        check_false "X where X = ((Int, Int), X) | `nil" "[ Int+ ]" ;
      ] ;

    "basic_w_var",
      [
        (* FAILED *)
        check "('a, `nil)" "[ 'a ]" ;

        (* FAILED *)
        check "('a, ('b, `nil))" "[ 'a; 'b ]" ;

        (* FAILED *)
        check "(('a, `nil), 'b)" "([ 'a ], 'b)" ;

        check_false "(('a, `nil), 'b)" "[ 'a; 'b ]" ;
        check_false "('a, 'a)" "[ 'a; 'a ]" ;
      ] ;
    "rec_w_var",
      [
        (* FAILED *)
        check "X where X = ('a, X) | `nil" "[ 'a* ]" ;

        (* FAILED *)
        check "X where X = ('a, ('a, X)) | `nil" "[ 'a+ ]" ;

        check_false "X where X = ('a, X) | `nil" "[ [ 'a* ]* ]" ;
        check_false "X where X = (('a, 'a), X) | `nil" "[ 'a+ ]" ;
      ] ;
  ]