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
  let rec loop global =
    match Syntax.Parser.typ_decl ~debug:Format.err_formatter lexbuf with
    | Ok (has_next, td) ->
      let td, nglobal = Syntax.Typing.type_decl global td in
      let t0 = Unix.gettimeofday () in
      let is_empty = Stt.Typ.is_empty td.typ in
      let t1 = Unix.gettimeofday () in
      let open Format in
      eprintf "@[%a@]@\n" Syntax.Typing.GlobalDecl.pp td;
      eprintf "--@\n@[%a@]@\nis_empty: %b in %fms@\n----@\n"
        Stt.Typ.pp td.typ
        is_empty   (1000. *. (t1 -. t0));
      if has_next then loop nglobal
    | Error msg -> Printf.eprintf "ERROR: %s\n" msg
  in  loop Syntax.Typing.default


let () = main ()