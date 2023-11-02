open Stt

exception Type_error of string

let error ?loc fmt =
  Format.(kasprintf (fun s -> raise (Type_error (
      asprintf "%a: %s" (pp_print_option Loc.pp) loc s
    ))))
    fmt

module Name = Base.Hstring
module HName = Hashtbl.Make(Name)

let mk_name = let i = ref 0 in
  fun () -> incr i;
    Name.make ("#RE_" ^ (string_of_int !i))

let is_gen_name n =
  let s = Name.(!!n) in
  String.length s = 0 || s.[0] = '#' 

module Re_compile : sig
  val compile : Ast.re -> Ast.typ_expr
end = struct
  (* Glushkov automata, aka Berry-Sethi construction. Our alphabet is the set of
     occurrences of (Re_typ t) in the regexp. No need to linerize since each t
     is localized and therefore unique.
  *)
  module TSet = Set.Make (Name)

  let rec first re = match snd re with
    | `Re_epsilon -> TSet.empty
    | `Re_typ t -> TSet.singleton t
    | `Re_alt (r1, r2) -> TSet.union (first r1) (first r2)
    | `Re_concat ((eps1,_) as r1, r2) when eps1 -> TSet.union (first r1) (first r2)
    | `Re_concat (r1, _) -> first r1
    | `Re_star r -> first r

  let rec last re = match snd re with
    | `Re_epsilon -> TSet.empty
    | `Re_typ t -> TSet.singleton t
    | `Re_alt (r1, r2) -> TSet.union (last r1) (last r2)
    | `Re_concat (r1, (eps2,_ as r2)) when eps2 -> TSet.union (last r2) (last r1)
    | `Re_concat (_, r2) -> last r2
    | `Re_star r -> last r

  let rec follow t re = match snd re with
    | `Re_epsilon | `Re_typ _ -> TSet.empty
    | `Re_alt (r1, r2) -> TSet.union (follow t r1) (follow t r2)
    | `Re_concat (r1, r2) ->
      TSet.union (follow t r1)
        (TSet.union (follow t r2)
           (if TSet.mem t (last r1) then first r2 else TSet.empty))
    | `Re_star r ->
      TSet.union (follow t r)
        (if TSet.mem t (last r) then first r else TSet.empty)

  let compile re =
    let open Loc in
    let names = HName.create 16 in
    let fresh t =
      let n = mk_name () in
      HName.add names n t; n
    in
    let rec unique re =
      match re.descr with
        Ast.Re_epsilon -> (true,`Re_epsilon)
      | Re_typ t -> (false,`Re_typ (fresh t))
      | Re_alt (r1, r2) ->
        let r1 = unique r1 in let r2 = unique r2 in
        (fst r1 || fst r2,`Re_alt (r1, r2))
      | Re_concat (r1, r2) ->
        let r1 = unique r1 in let r2 = unique r2 in
        (fst r1 && fst r2,`Re_alt (r1, r2))
      | Re_star r -> true,`Re_star (unique r)
    in
    let start = mk_name () in
    let rel = unique re in
    let first = first rel and last = last rel in
    let htypes = HName.create 16 in
    let nil = with_loc re.loc (Ast.Typ Stt.Builtins.nil) in
    let inst loc name =
      with_loc loc (Ast.Inst { call = with_loc loc name; args = []; def = None })
    in
    let add_trans q_in t =
      let t_orig = HName.find names t in
      let prod = copy_loc t_orig (Pair(t_orig, inst t_orig.loc t)) in
      let new_t = try copy_loc t_orig (Cup (HName.find htypes q_in, prod)) with Not_found -> prod in
      HName.replace htypes q_in new_t
    in
    let set_final q = HName.replace htypes q nil in
    let () = if fst rel then set_final start in
    let () = names
             |> HName.iter (fun t _ ->
                 if TSet.mem t last then set_final t;
                 if TSet.mem t first then add_trans start t;
                 TSet.iter (add_trans t) (follow t rel))
    in
    with_loc re.loc (Ast.Rec (inst re.loc start,
                              HName.fold (fun x te acc ->
                                  (with_loc re.loc x, te) :: acc)
                                htypes []))

end

module Env : sig
  type 'a t
  val empty : 'a t
  val add : Ast.lident -> 'a -> 'a t -> 'a t
  val replace : Ast.lident -> 'a -> 'a t -> 'a t
  val find : Ast.lident -> 'a t -> 'a
  val find_loc : Ast.lident -> 'a t -> 'a * Loc.t
  val find_unloc : Name.t -> 'a t -> 'a * Loc.t
  val find_opt : Ast.lident -> 'a t -> 'a option
  val find_loc_opt : Ast.lident -> 'a t -> ('a * Loc.t) option
  val find_unloc_opt : Name.t -> 'a t -> ('a * Loc.t) option

  val mem : Ast.lident -> 'a t -> bool
  val mem_unloc : Name.t -> 'a t -> bool
end =
struct
  open Name
  module M = Map.Make (Name)
  type 'a t = ('a * Loc.t) M.t
  let empty = M.empty

  let add n v env =
    M.update n.Loc.descr
      (function None -> Some (v,n.Loc.loc)
              | Some (_, other) ->
                error ~loc:n.Loc.loc "Name %s is already defined at %a" !!(n.Loc.descr)
                  Loc.pp other)
      env

  let replace n v env : 'a t =
    M.add n.Loc.descr (v, n.Loc.loc) env

  let find_unloc n env =
    try (M.find n env) with Not_found -> error "Name %s is undefined" !!n

  let find_loc n env = find_unloc n.Loc.descr env

  let find n env = fst (find_loc n env)

  let find_unloc_opt n env =
    M.find_opt n env

  let find_loc_opt n env =
    find_unloc_opt n.Loc.descr env

  let find_opt n env =
    Option.map fst (find_loc_opt n env)

  let mem_unloc = M.mem

  let mem n = M.mem n.Loc.descr

end
type global_decl = {
  decl : Ast.typ_decl;
  vars : (Name.t * Var.t) list;
  typ : Typ.t;
  recs : Ast.typ_expr Env.t
}
type global = global_decl Env.t

let derecurse global env te =
  let open Ast in
  let open Loc in
  let recs = ref Env.empty in
  let rec loop collect params env te =
    let do_loop = loop collect params env in
    copy_loc te @@
    match te.descr with
      Typ _ as d -> d
    | Pair (t1, t2) -> Pair (do_loop t1, do_loop t2)
    | Arrow (t1, t2) -> Arrow (do_loop t1, do_loop t2)
    | Cup (t1, t2) -> Cup (do_loop t1, do_loop t2)
    | Cap (t1, t2) -> Cap (do_loop t1, do_loop t2)
    | Diff (t1, t2) -> Diff (do_loop t1, do_loop t2)
    | Neg t -> Neg (do_loop t)
    | Var n as d -> begin
        match Env.find_opt n params with
          None -> d
        | Some e -> (do_loop e).descr end
    | Regexp re -> 
      (do_loop (Re_compile.compile re)).descr 
    | Rec (t, l) ->
      let todo, env' =
        List.fold_left (fun (atodo,aenv) (n, d) ->
            match Env.find_loc_opt n aenv with
              None ->
              let inst = { call=n; args=[]; def=None } in
              inst :: atodo, Env.add n (d,inst) aenv
            | Some (_, loc) ->
              error ~loc:n.loc
                "Multiple definitions of recursive type variable %s, the previous one was at %a"
                Name.(!!(n.descr)) Loc.pp loc) ([], env) l
      in
      let nt = loop collect params env' t in
      let nl = List.rev_map (fun (n, te) -> (n, loop collect params env' te)) l in
      let nl = List.fold_left2 (fun acc inst ((x, nte) as v) ->
          inst.def <- Some nte;
          if collect && not (is_gen_name x.descr) then recs := Env.add x nte !recs;
          v :: acc) [] todo nl
      in
      Rec(nt, nl)
    | Inst ({ call; args; def=None } as rinst) ->
      let d =
        match args, Env.find_opt call env with
          _, None -> inline_global collect params env rinst None
        | [], Some (_, inst) -> Inst inst
        | _ -> error ~loc:call.loc "Name %s is a recursive type variable, it cannot be instantiated"
                 Name.(!!(call.descr))
      in d
    | Inst _ as d -> d
    | From (x, ({def = None; _} as rinst)) ->
      inline_global collect params env rinst (Some x)
    | From _ as d -> d

  and inline_global collect params env ({call;args; _} as rinst) teopt =
    let args = List.map (loop collect params env) args in
    let decl = Env.find call global in
    let nparams =
      try
        List.fold_left2 (fun acc x arg ->
            Env.add x arg acc
          ) Env.empty
          decl.decl.params args
      with Invalid_argument _ ->
        let num_params = List.length decl.decl.params in
        error ~loc:call.loc "Parametric type %s expects %d argument%s but was applied to %d"
          Name.(!!(call.descr))
          num_params
          (if num_params < 2 then "" else "s")
          (List.length args)
    in
    let te = match teopt with
        None -> decl.decl.expr
      | Some n -> Env.find n decl.recs
    in
    let d = loop false nparams env te in
    rinst.def <- Some d;d.descr
  in
  let res = loop true Env.empty env te in
  res, !recs

  (*
let epxand te =
  let open Loc in
  let open Ast in
  let memo = HName.create 16 in
  let rec loop te =
    with_loc te.loc @@
    match te.descr with
    | (Typ _ | Pair _ | Arrow _ | Var _ | Regexp _) as d -> d
    | Cup (t1, t2) -> Cup (loop t1, loop t2)
    | Cap (t1, t2) -> Cap (loop t1, loop t2)
    | Diff (t1, t2) -> Diff (loop t1, loop t2)
    | Neg t -> Neg (loop t)
    | Inst _ as d -> (follow d).descr
    (*
      if HName.mem memo call.descr then
        error ~loc:call.loc "The definition of %s is not contractive"
          Name.(!!(call.descr));
      HName.add memo call.descr ();
      let d = loop (follow t) in
      r.def <- Some d;
      d.descr
    | *)
*)