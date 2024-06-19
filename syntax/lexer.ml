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

let val_char c =
  if c <= '9' then Char.code c - 48
  else if c <= 'A' then Char.code c - 55
  else Char.code c - 87
let decode_unicode_esc s =
  let total = ref 0 in
  for i = 3 to String.length s - 2 do
    total := 16 * !total + val_char s.[i];
  done;
  (Uchar.of_int !total)

let ident2 = [%sedlex.regexp? (xml_letter | '_' | xml_digit) ]

(* char of identifers after the first one *)
let ident = [%sedlex.regexp? (xml_letter | '_'), Star ident2]

module Buf =
struct
  type t = { mutable chars : Uchar.t array;
             mutable length : int }

  let empty_array n = Array.make n Uchar.rep
  let create () =
    { chars = empty_array 32; length = 0}

  let reset t =
    t.chars <- empty_array 32;
    t.length <- 0

  let length t = t.length

  let push t c =
    Format.eprintf "PUSH: %c@\n%!" (Uchar.to_char c);
    let len = t.length in
    if len = Array.length t.chars then begin
      let nchars = empty_array (len * 2) in
      Array.blit t.chars 0 nchars 0 len;
      t.chars <- nchars;
    end;
    t.chars.(len) <- c;
    t.length <- len + 1
  let extract t = Array.sub t.chars 0 t.length
end
let str_buf = Buf.create ()

let rec lexer lexbuf =
  let open Sedlexing in
  match%sedlex lexbuf with
  (* Whitespaces and phrase separators *)
  | ' ' | '\t' | '\r' -> lexer lexbuf
  | '\n' -> new_line lexbuf; lexer lexbuf
  | '#',Star (Compl '\n'), ('\n'| eof) ->new_line lexbuf; lexer lexbuf
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
  | number -> INT (Z.of_string (Utf8.lexeme lexbuf))
  | '"'   -> Buf.reset str_buf; string lexbuf
  | '\'', normal_char, '\'' -> CHAR (sub_lexeme lexbuf 1 1).(0)
  | '\'', unicode_esc_char, '\'' ->
    CHAR (decode_unicode_esc (Utf8.lexeme lexbuf))
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
           (lexing_positions lexbuf)
           "Unexpected character '%s'" (Utf8.lexeme lexbuf)

and string lexbuf =
  let open Sedlexing in
  match%sedlex lexbuf with
  | '"' -> STRING (Buf.extract str_buf)
  | normal_char ->
    Buf.push str_buf (lexeme lexbuf).(0);
    string lexbuf
  | unicode_esc_char ->
    Buf.push str_buf (decode_unicode_esc (Utf8.lexeme lexbuf));
    string lexbuf
  | "\\t" -> Buf.push str_buf (Uchar.of_char '\t'); string lexbuf
  | "\\n" -> Buf.push str_buf (Uchar.of_char '\n'); string lexbuf
  | "\\\\" -> Buf.push str_buf (Uchar.of_char '\\'); string lexbuf
  | "\\'" -> Buf.push str_buf (Uchar.of_char '\''); string lexbuf
  | "\\b" -> Buf.push str_buf (Uchar.of_char '\b'); string lexbuf
  | "\\\"" -> Buf.push str_buf (Uchar.of_char '"'); string lexbuf
  | eof -> lexical_error (lexing_positions lexbuf)
             "Unterminated string"
  | _ -> lexical_error
           (lexing_positions lexbuf)
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
