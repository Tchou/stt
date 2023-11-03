let token_to_string t =
  let open Tokens in
  let open Format in
  match t with
  | AND -> "AND"
  | CAP -> "CAP"
  | CHAR  (c) -> asprintf "CHAR (%a)" Stt.Char.pp_char c
  | COMMA -> "COMMA"
  | CUP -> "CUP"
  | DIFF -> "DIFF"
  | ENUM  (s) -> asprintf "ENUM (%a)" Stt.Enum.pp_enum s
  | EOF -> "EOF"
  | EOP -> "EOP"
  | EQUAL -> "EQUAL"
  | FROM -> "FROM"
  | IDENT (s) -> asprintf "IDENT (%s)" Stt.Base.Hstring.(!!s)
  | INT (z) -> asprintf "INT (%s)" (Z.to_string z)
  | LP -> "LP"
  | LSB -> "LSB"
  | MINUSGT -> "MINUSGT"
  | MINUSMINUS -> "MINUSMINUS"
  | NOT -> "NOT"
  | PLUS -> "PLUS"
  | QMARK -> "QMARK"
  | RP -> "RP"
  | RSB -> "RSB"
  | SEMI -> "SEMI"
  | STAR -> "STAR"
  | TYPE -> "TYPE"
  | VAR (s) -> asprintf "VAR ('%s)" Stt.Base.Hstring.(!!s)
  | WHERE -> "WHERE"