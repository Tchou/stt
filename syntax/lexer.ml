open Tokens

let hstr = Stt.Base.Hstring.cons

let lexical_error _pos fmt = (* Todo change *)
  Format.kasprintf (fun s -> failwith s) fmt

let keywords =
  (* List keywords here to reduce the automaton size *)
  [ ("and", AND);
    ("from", FROM);
    ("type", TYPE );
    ("where", WHERE)]
  |> List.to_seq
  |> Hashtbl.of_seq

let start_of_phrase_keyword =
  [ (TYPE, true )]
  |> List.to_seq
  |> Hashtbl.of_seq

let ident_or_keyword s =
  try Hashtbl.find keywords s with Not_found -> IDENT (hstr s)

let digit = [%sedlex.regexp? '0' .. '9']
let hexdigit = [%sedlex.regexp? '0' .. '9'| 'A' .. 'F' | 'a' .. 'f' ]
let number = [%sedlex.regexp? Opt '-', Plus digit]
let normal_char =
  [%sedlex.regexp?
      0x20 .. 0x26 (* 0x27 : ' *)
               | 0x28 .. 0x53 (* 0x54 : \ *)
               | 0x55 .. 0x7e
               | 0xa0 .. 0x1ffff (* non control chars *)
  ]

let symbol_char = [%sedlex.regexp? Chars " ()[]|&-+~\\,;*+?=>"]
let unicode_esc_char = [%sedlex.regexp?
    "\\u{", Rep(hexdigit,2 .. 6),'}' (* unicode escape *)
]
let ident2 = [%sedlex.regexp? (xml_letter | '_' | xml_digit) ]

(* char of identifers after the first one *)
let ident = [%sedlex.regexp? (xml_letter | '_'), Star ident2]

let rec lexer lexbuf =
  let open Sedlexing in
  match%sedlex lexbuf with
  (* Whitespaces and phrase separators *)
  | ' ' | '\t' | '\r' -> lexer lexbuf
  | '\n' -> new_line lexbuf; lexer lexbuf
  | '#',Star (Compl '\n'), '\n' ->new_line lexbuf; lexer lexbuf
  | ";;" -> EOP


  (* Symbols *)
  | "->" -> MINUSGT
  | "--" -> MINUSMINUS
  | "("  -> LP
  | ")"  -> RP
  | "["  -> LSB
  | "]"  -> RSB
  | "&"  -> CAP
  | "|"  -> CUP
  | "\\" -> DIFF
  | "~" -> NOT
  | "," -> COMMA
  | ";" -> SEMI
  | "*" -> STAR
  | "+" -> PLUS
  | "?" -> QMARK
  | "=" -> EQUAL

  (* Constants *)
  | digit  -> INT (Z.of_string (Utf8.lexeme lexbuf))
  | '\'', normal_char, '\'' -> CHAR (sub_lexeme lexbuf 1 1).(0)
  | '\'', unicode_esc_char, '\'' ->
    let s = Utf8.lexeme lexbuf in
    let n = String.sub s 3 (String.length s - 4) in
    CHAR (Uchar.of_int (int_of_string ("0x" ^ n)))
  | "'\\t'" -> CHAR (Uchar.of_char '\t')
  | "'\\n'" -> CHAR (Uchar.of_char '\n')
  | "'\\\\'" -> CHAR (Uchar.of_char '\\')
  | "'\\''" -> CHAR (Uchar.of_char '\'')
  | "'\\b'" -> CHAR (Uchar.of_char '\b')
  | '\'', normal_char, Plus(Sub(Compl(ident2),symbol_char)),'\'' ->
    lexical_error
      (Sedlexing.lexing_positions lexbuf)
      "Caracter litteral %s contains more than one code point" (Utf8.lexeme lexbuf)

  (* Atoms and polymorphic variables *)
  | ('`'|'\''),ident ->
    let s = Utf8.lexeme lexbuf in
    let is_atom = s.[0] = '`' in
    let ss = hstr (String.(sub s 1 (length s - 1))) in
    if is_atom then ENUM ss else VAR ss

  (* Identifiers names *)
  | ident -> ident_or_keyword (Utf8.lexeme lexbuf)

  (* EOF and Unknown *)
  | eof -> EOF
  | _ -> lexical_error
           (Sedlexing.lexing_positions lexbuf)
           "Unexpected character '%s'" (Utf8.lexeme lexbuf)

let lexer =
  let queue = ref None in
  fun lexbuf ->
    match !queue with
      Some token -> queue := None; token
    | None ->
      let token = lexer lexbuf in
      if Hashtbl.mem start_of_phrase_keyword token then begin
        queue := Some token; EOP
      end else token
