let parse_type env s =
  let lexbuf = Sedlexing.Utf8.from_string s in
  match Syntax.Parser.typ_decl lexbuf with
    Error msg -> Format.eprintf "ERROR: %s@\n" msg; exit 1
  | Ok (_, t_ast) ->
    let t,env' = Syntax.Typing.(type_decl env t_ast) in
    t.Syntax.Typing.typ, env'

open Stt
module Normal = Base.Cartesian.Make (Typ) (Typ)
let extract (n1, n2) = Typ.descr n1, Typ.descr n2

let any_star =
  let open Typ in
  let x = make () in
  let p = product (node any) x in
  let c = cup Builtins.nil p in
  let () = def x c in
  c

let format_auto fmt t =
  let open Format in
  let state = let s = ref ~-1 in
    fun () -> incr s; !s in
  let init = state () in
  let finals = ref [] in
  let states = ref [(t, init)] in
  let trans = ref [] in
  let rec loop t q =
    if Typ.subtype Builtins.nil t then finals := q :: !finals;
    states := (t, q) :: !states;
    let prod = Typ.VarProduct.(full_dnf (get t))
               |> Seq.map snd
               |> Seq.map (fun (pl, nl) ->
                   (List.map extract pl,
                    List.map extract nl)
                 )
    in
    let norm = Normal.normal prod in
    let todo = 
      norm |>
      List.fold_left (fun acc (t1, t2) ->
          let q', acc = match List.find_opt (fun (t', _) -> Typ.equiv t2 t') (!states@acc) with
              None -> let q = state () in q, (t2,q)::acc
            | Some (_, q') -> q', acc
          in
          let l1 = 
            let s = asprintf "%a" Syntax.Pretty.pp t1 in
            String.split_on_char '|' s
            |> List.map String.trim
          in
          List.iter (fun s -> trans := (q, s, q') :: !trans) l1;
          acc
        ) []
    in
    List.iter (fun (t,q) -> loop t q) todo
  in
  let () = loop t init in
  fprintf fmt "%d@\n" (state());
  pp_print_list ~pp_sep:(fun fmt () -> Format.pp_print_string fmt ",")
    pp_print_int
    fmt  !finals;
  fprintf fmt "@\n";
  List.iter (fun (q, s, q') ->
      fprintf fmt "%d,%s,%d@\n" q s q') (List.sort compare !trans)



let dump_auto env path s =
  let t, env' = parse_type env s in
  if not (Typ.subtype t any_star) then
    Format.eprintf "Ignoring %s, which is not a subtype of [Any*]@\n" s
  else begin
    let c_out = open_out_bin path in
    let fmt = Format.formatter_of_out_channel c_out in
    Format.fprintf fmt "%s@\n" s;
    format_auto fmt t;
    Format.pp_print_flush fmt ();
    close_out c_out;
  end;
  env'

(*
Syntax of types:
type t = [ regexp1 ] | [ regexp2 ] | ...

Syntax of regexps = 
- symbols can be:
    - Int, Bool
    - variants: `A, `B, `foo, (like OCaml's polymorphic variants). Note: Bool ≡ `false | `true
    - any previously defined type

- Union |, concaténation ;
- *, +, ?
ou expression entièrement vide

*)
let main () =
  let l = [ 
    "type t0 = [ `A;`B;`C ]";
    "type t1 = [ Int*; Int ]";
    "type t2 = [ (Int|Bool)*; Int ]";
    "type t3 = [ Bool;Bool+ ]";
    "type t4 = [ ((`A|`B); Int)+ ]";
    "type t5 = t0 | [ Bool ]";
    "type t6 = t0 | []";
    "type t7 = []";
    "type t8 = [ ((`A|`B)*; `A*;`C)+ ]";

    ]
  in
  List.fold_left (fun (i, env) s ->
      let path = Format.sprintf "test/auto%03d.txt" i in
      let env' = dump_auto env path s in
      (i+1, env')
    ) (0, Syntax.Typing.default) l
  |> ignore


let () = main ()