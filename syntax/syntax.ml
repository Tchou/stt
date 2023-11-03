
module Lexer = Lexer
module Parser : sig
  type 'a parser = ?debug:Format.formatter -> Sedlexing.lexbuf -> (bool * 'a, string) result
  val typ_decl : Ast.typ_decl parser
  val typ_expr : Ast.typ_expr parser
end
=
struct
  type 'a parser = ?debug:Format.formatter -> Sedlexing.lexbuf -> (bool * 'a, string) result

  open Parser.MenhirInterpreter

  let consume gram ?debug lexer =
    let pp_pos fmt p = Format.fprintf fmt "%d:%d" p.Lexing.pos_lnum
        (p.Lexing.pos_cnum -p.Lexing.pos_bol)
    in
    let rec loop passed_eof get_token cp =
      match cp with
        InputNeeded _ ->
        let (token, pos_start, pos_end) as pos = get_token () in
        let () =
          match debug with
            None -> ()
          | Some fmt ->
            Format.fprintf fmt "@[%a-%a: %s@]@\n"
              pp_pos pos_start
              pp_pos pos_end
              (Misc.token_to_string token)
        in
        loop (token = Tokens.EOF) get_token (offer cp pos)
      | Rejected -> Result.error "Parsing error"
      | Accepted e -> Result.ok @@ (not passed_eof, e)
      | HandlingError _ -> Result.error "Parsing error2"
      | _ -> loop passed_eof get_token (resume cp)
    in
    fun lexbuf ->
      let start, _ = Sedlexing.lexing_positions lexbuf in
      let get_token = Sedlexing.with_tokenizer lexer lexbuf in
      let init = gram start in
      loop false get_token init

  let typ_decl ?debug = consume ?debug Parser.Incremental.typ_decl Lexer.lexer
  let typ_expr ?debug = consume ?debug Parser.Incremental.typ_expr Lexer.lexer

end
module Misc = Misc
module Ast = Ast
module Tokens = Tokens
module Typing = Typing