exception Except of string

let parse_type ?(debug : bool = false) 
                (s : string) : Stt.Typ.t =
  let lexbuf = Sedlexing.Utf8.from_string s in
  let fmt = 
    if debug then
      Some(Format.err_formatter)
    else
      None
  in
  match Syntax.Parser.typ_decl ?debug:fmt lexbuf with
  | Error msg -> raise @@ Except msg
  | Ok (_, t_ast) ->
    let t, _ = Syntax.Typing.(type_decl Syntax.Typing.default t_ast) in
    t.Syntax.Typing.typ