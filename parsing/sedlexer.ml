open Parser


let digit = [%sedlex.regexp? '0' .. '9']
let hexdigit = [%sedlex.regexp? '0' .. '9'| 'A' .. 'F' | 'a' .. 'f' ]
let number = [%sedlex.regexp? Opt '-', Plus digit]
let normal_char = [%sedlex.regexp?
    0x20 .. 0x26 (* 0x27 : ' *)
                               | 0x28 .. 0x53 (* 0x54 : \ *)
                               | 0x55 .. 0x7e
                               | 0xa0 .. 0x1ffff (* non control chars *)
]
let unicode_esc_char = [%sedlex.regexp?
    "\\u{", Rep(hexdigit,2 .. 6),'}' (* unicode escape *)
]
let ident = [%sedlex.regexp? (xml_letter | '_'), Star (xml_letter | '_' | xml_digit)]

let rec lexer lexbuf =
  match%sedlex lexbuf with
  | ' ' | '\t' | '\r' -> lexer lexbuf
  | '\n' -> Sedlexing.new_line lexbuf; lexer lexbuf
  | digit  -> INT (Z.of_string (Sedlexing.Utf8.lexeme lexbuf))
  | '\'', normal_char, '\'' -> CHAR (Sedlexing.sub_lexeme lexbuf 1 1).(0)
  | '\'', unicode_esc_char, '\'' ->
    let s = Sedlexing.Utf8.lexeme lexbuf in
    let n = String.sub s 3 (String.length s - 4) in
    CHAR (Uchar.of_int (int_of_string ("0x" ^ n)))
  | "'\\t'" -> CHAR (Uchar.of_char '\t')
  | "'\\n'" -> CHAR (Uchar.of_char '\n')
  | "'\\\\'" -> CHAR (Uchar.of_char '\\')
  | "'\\''" -> CHAR (Uchar.of_char '\'')
  | Opt('`'|'\''),ident ->
    let s = Sedlexing.Utf8.lexeme lexbuf in
    let i,k = match s.[0] with
      | '`' -> 1,`atom
      | '\'' -> 1,`var | _ -> 0,`name
    in begin
      let ss = Stt.Base.Hstring.make (String.sub s i (String.length s - i)) in
      match k with
        `var -> VAR ss
      |`atom -> ATOM ss
      | `name -> NAME ss
    end

  | "->" -> MINUSGT
  | "--" -> MINUSMINUS
  | "("  -> LP
  | ")"  -> RP
  | "["  -> LSB
  | "]"  -> RSB
  | "&"  -> AND
  | "|"  -> OR
  | "\\" -> DIFF
  | "~" -> NOT
  | "," -> COMMA
  | ";" -> SEMI
  | "*" -> STAR
  | "+" -> PLUS
  | "?" -> QMARK
  | _ -> failwith ("Unknown character '" ^ Sedlexing.Utf8.lexeme lexbuf ^ "'")