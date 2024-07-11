open Stt
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


let alpha_renaming (t : Typ.t) (t' : Typ.t) =
  let vp, vn = Typ.vars t in
  let v = Var.Set.union vp vn in
  let var_map = Var.Set.fold (
      fun v acc -> (v.Var.name, v) :: acc
    ) v []
  in
  let vp', vn' = Typ.vars t' in
  let v' = Var.Set.union vp' vn' in
  let subst, s = Var.Set.fold (
      fun v (acc_l, acc_s) ->
        let n_var, acc_s =
          match List.assoc_opt v.Var.name var_map with
          | Some n_var ->
            n_var, Format.asprintf "%s %a->%a" acc_s Var.dump v Var.dump n_var
          | None -> v, Format.asprintf "%s %a" acc_s Var.dump v
        in
        Subst.add v (Typ.var n_var) acc_l, acc_s
    ) v' (Subst.of_list [], "")
  in
  Subst.apply subst t', s