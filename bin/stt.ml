open Arg

let usage_msg = 
  Format.sprintf "usage: %s <file>\n" Sys.argv.(0)


let input_file = ref ""

let anon s =
  if !input_file = "" then 
  input_file := s
  else
    raise (Bad ("Don't know what to do with " ^ s))

let main () = 
  parse [] anon usage_msg;
  let cin = open_in !input_file in
  let lexbuf = Sedlexing.Utf8.from_channel cin in
  let init_pos,_ = Sedlexing.lexing_positions lexbuf in
  let start = Syntax.Parser.Incremental.single_typ_decl init_pos in
  let lexer = Sedlexing.with_tokenizer Syntax.Sedlexer.lexer lexbuf in
  let rec lex_until pos =
    let open Syntax.Parser.MenhirInterpreter in
    match pos with
    | InputNeeded _ ->
      let (token, _, _) as tok = lexer () in
      Format.eprintf ">> %s\n%!" (Syntax.Misc.token_to_string token);
      lex_until (offer pos tok)
    | Rejected -> failwith "?"
    | Accepted _ -> Printf.printf "Parsed !\n%!"
    | e -> lex_until (resume e)
  in
  while true do
    lex_until start
  done




let () = main ()