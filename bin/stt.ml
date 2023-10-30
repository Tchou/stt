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
  let rec loop () =
    match Syntax.Parser.typ_decl ~debug:Format.err_formatter lexbuf with
      Ok None ->  ();
    | Ok (Some _) -> Printf.eprintf "PARSED\n%!"; loop ()
    | Error msg -> Printf.eprintf "ERROR: %s\n" msg
  in  loop ()



let () = main ()