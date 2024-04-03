open Stt

exception Type_error of string

let error ?loc fmt =
  Format.(kasprintf (fun s -> raise (Type_error (
      asprintf "%a: %s" (pp_print_option Loc.pp) loc s
    ))))
    fmt

module IdentTable = Hashtbl.Make (Ident)
module IdentSet = Set.Make (Ident)
module IdentMap = Map.Make (Ident)

let mk_name = let i = ref 0 in
  fun () -> incr i;
    Ident.cons ("#RE_" ^ (string_of_int !i))

let is_gen_name n =
  let s = Ident.(!!n) in
  String.length s = 0 || s.[0] = '#'

module Re_compile : sig
  val compile : Ast.Located.re -> Ast.Located.typ
end = struct
  (* Glushkov automata, aka Berry-Sethi construction. Our alphabet is the set of
     occurrences of (Re_typ t) in the regexp. No need to linerize since each t
     is localized and therefore unique.
  *)

  let rec first re = match snd re with
    | `Re_epsilon -> IdentSet.empty
    | `Re_typ t -> IdentSet.singleton t
    | `Re_alt (r1, r2) -> IdentSet.union (first r1) (first r2)
    | `Re_concat ((eps1,_) as r1, r2) when eps1 -> IdentSet.union (first r1) (first r2)
    | `Re_concat (r1, _) -> first r1
    | `Re_star r -> first r

  let rec last re = match snd re with
    | `Re_epsilon -> IdentSet.empty
    | `Re_typ t -> IdentSet.singleton t
    | `Re_alt (r1, r2) -> IdentSet.union (last r1) (last r2)
    | `Re_concat (r1, (eps2,_ as r2)) when eps2 -> IdentSet.union (last r2) (last r1)
    | `Re_concat (_, r2) -> last r2
    | `Re_star r -> last r

  let rec follow t re = match snd re with
    | `Re_epsilon | `Re_typ _ -> IdentSet.empty
    | `Re_alt (r1, r2) -> IdentSet.union (follow t r1) (follow t r2)
    | `Re_concat (r1, r2) ->
      IdentSet.union (follow t r1)
        (IdentSet.union (follow t r2)
           (if IdentSet.mem t (last r1) then first r2 else IdentSet.empty))
    | `Re_star r ->
      IdentSet.union (follow t r)
        (if IdentSet.mem t (last r) then first r else IdentSet.empty)

  let compile re =
    let open Loc in
    let names = IdentTable.create 16 in
    let fresh t =
      let n = mk_name () in
      IdentTable.add names n t; n
    in
    let rec unique re =
      match re.descr with
        `Re_epsilon -> (true,`Re_epsilon)
      | `Re_typ t -> (false,`Re_typ (fresh t))
      | `Re_alt (r1, r2) ->
        let r1 = unique r1 in let r2 = unique r2 in
        (fst r1 || fst r2,`Re_alt (r1, r2))
      | `Re_concat (r1, r2) ->
        let r1 = unique r1 in let r2 = unique r2 in
        (fst r1 && fst r2,`Re_concat (r1, r2))
      | `Re_star r -> true,`Re_star (unique r)
    in
    let start = mk_name () in
    let rel = unique re in
    let first = first rel and last = last rel in
    let htypes = IdentTable.create 16 in
    let nil = with_loc re.loc (`Typ Stt.Builtins.nil) in
    let node loc name =
      let name = with_loc loc name in
      with_loc loc (`Node (ref (`Inst (name, []))))
    in
    let add_trans q_in t =
      let t_orig = IdentTable.find names t in
      let prod = copy_loc t_orig (`Pair(t_orig, node t_orig.loc t)) in
      let new_t = try copy_loc t_orig (`Cup [IdentTable.find htypes q_in; prod]) with Not_found -> prod in
      IdentTable.replace htypes q_in new_t
    in
    let set_final q = IdentTable.replace htypes q nil in
    let () = if fst rel then set_final start in
    let () = names
             |> IdentTable.iter (fun t _ ->
                 if IdentSet.mem t last then set_final t;
                 if IdentSet.mem t first then add_trans start t;
                 IdentSet.iter (add_trans t) (follow t rel))
    in
    with_loc re.loc
      (`Node (ref (`Rec (node re.loc start,
                         IdentTable.fold (fun x te acc ->
                             (with_loc re.loc x, te) :: acc)
                           htypes []))))

end

module Env : sig
  type 'a t
  val empty : 'a t
  val add : Ast.Located.ident -> 'a -> 'a t -> 'a t
  val replace : Ast.Located.ident -> 'a -> 'a t -> 'a t
  val find : Ast.Located.ident -> 'a t -> 'a
  val find_loc : Ast.Located.ident -> 'a t -> 'a * Loc.t
  val find_unloc : Ident.t -> 'a t -> 'a * Loc.t
  val find_opt : Ast.Located.ident -> 'a t -> 'a option
  val find_loc_opt : Ast.Located.ident -> 'a t -> ('a * Loc.t) option
  val find_unloc_opt : Ident.t -> 'a t -> ('a * Loc.t) option

  val mem : Ast.Located.ident -> 'a t -> bool
  val mem_unloc : Ident.t -> 'a t -> bool
  val to_seq : 'a t -> (Ident.t * ('a * Loc.t)) Seq.t
end =
struct
  open Ident
  type 'a t = ('a * Loc.t) IdentMap.t
  let empty = IdentMap.empty
  let to_seq e = IdentMap.to_seq e

  let add n v env =
    IdentMap.update n.Loc.descr
      (function None -> Some (v,n.Loc.loc)
              | Some (_, other) ->
                error ~loc:n.Loc.loc "Ident %s is already defined at %a" !!(n.Loc.descr)
                  Loc.pp other)
      env

  let replace n v env : 'a t =
    IdentMap.add n.Loc.descr (v, n.Loc.loc) env

  let find_unloc n env =
    try (IdentMap.find n env) with Not_found -> error "Ident %s is undefined" !!n

  let find_loc n env = find_unloc n.Loc.descr env

  let find n env = fst (find_loc n env)

  let find_unloc_opt n env =
    IdentMap.find_opt n env

  let find_loc_opt n env =
    find_unloc_opt n.Loc.descr env

  let find_opt n env =
    Option.map fst (find_loc_opt n env)

  let mem_unloc = IdentMap.mem

  let mem n = IdentMap.mem n.Loc.descr

end
type global_decl = {
  decl : Ast.Located.decl;
  vars : (Ident.t * Stt.Var.t) list;
  typ : Stt.Typ.t;
  recs : Ast.Located.typ Env.t;
}

module GlobalDecl (*: Stt.Base.Common.T with type t = global_decl *)=
struct
  type t = global_decl
  let name = "GlobalDecl"
  let compare a b =
    let na = a.decl.Ast.name in
    let nb = b.decl.Ast.name in
    let open Stt.Base.Common.Let in
    let<> () = Ident.compare na.Loc.descr nb.Loc.descr in
    Loc.compare na.Loc.loc nb.Loc.loc

  let equal a b = compare a b = 0
  let hash a = Hashtbl.hash a

  let pp fmt a =
    let open Format in
    let pp_list pe fmt l =
      pp_print_list ~pp_sep:(fun fmt () -> pp_print_string fmt "," ) pe fmt l
    in
    fprintf fmt "@[";
    fprintf fmt "@[Loc: %a@]@\n" Loc.pp a.decl.Ast.name.Loc.loc;
    fprintf fmt "@[Ident: %a" Ident.pp a.decl.Ast.name.Loc.descr;
    if a.vars <> [] then
      fprintf fmt " (%a)" (pp_list
                             (fun fmt (v, _) -> fprintf fmt "'%a" Ident.pp v)) a.vars;
    fprintf fmt "@]@\n";
    fprintf fmt "@[Recs: @[%a@]@]@\n" (pp_list Ident.pp)
      (a.recs |> Env.to_seq |> Seq.map fst |> List.of_seq)

end
type global = GlobalDecl.t Env.t
let empty = Env.empty

let enter_builtin name t (env : global) : global =
  let open Ast in
  let dummy_decl = {decl = {
      name = Loc.(with_loc dummy) name;
      params = [];
      expr = Loc.(with_loc dummy) (`Typ t) } ;
     vars = [];
     typ = t;
     recs = Env.empty }
  in
  Env.add dummy_decl.decl.name dummy_decl env


let default =
  List.fold_left (fun acc (n, t) -> enter_builtin n t acc)
    empty
    Stt.Builtins.by_names

let dummy_expr : Ast.Located.typ =
  Loc.(with_loc dummy (`Typ Stt.Typ.empty))

let derecurse global te =
  let open Ast in
  let open Loc in
  let recs = ref Env.empty in
  let rec loop collect params env te =
    let do_loop = loop collect params env in
    copy_loc te @@
    match te.descr with
      `Typ _ as d -> d
    | `Pair (t1, t2) -> `Pair (do_loop t1, do_loop t2)
    | `Arrow (t1, t2) -> `Arrow (do_loop t1, do_loop t2)
    | `Cup l -> `Cup (List.map do_loop l)
    | `Cap l -> `Cap (List.map do_loop l)
    | `Diff (t1, t2) -> `Diff (do_loop t1, do_loop t2)
    | `Neg t -> `Neg (do_loop t)
    | `Var n as d -> begin
        match Env.find_opt n params with
          None -> d
        | Some e -> (do_loop e).descr end
    | `Regexp re ->
      (do_loop (Re_compile.compile re)).descr
    | `Node r -> `Node (loop_node collect params env r)
    | `Extra _ -> .
  and loop_node collect params env r =
    let rnode = !r in
    match rnode with
    | `Rec (t, l) ->
      let new_r = ref (`Expr dummy_expr) in
      r := !new_r;
      let todo, env' =
        List.fold_left (fun (atodo, aenv) (n, d) ->
            match Env.find_loc_opt n aenv with
              None ->
              let rd = ref (`Expr dummy_expr) in
              rd :: atodo, Env.add n (d, rd) aenv
            | Some (_, loc) ->
              error ~loc:n.loc
                "Multiple definitions of recursive type variable %s, the previous one was at %a"
                Ident.(!!(n.descr)) Loc.pp loc) ([], env) l
      in
      let nt = loop collect params env' t in
      let nl = List.rev_map (fun (n, te) -> (n, loop collect params env' te)) l in
      let () = List.iter2 (fun rd ((x, nte)) ->
          rd := `Expr nte;
          if collect && not (is_gen_name x.descr) then recs := Env.add x nte !recs
        ) todo nl
      in new_r := `Expr nt; new_r
    | `Inst (x, args) -> begin
        match args, Env.find_opt x env with
          _, None -> let new_r = ref (`Expr dummy_expr) in
          inline_global collect params env x args new_r None
        | [], Some (_, r) -> r
        | _ -> error ~loc:x.loc "Ident %s is a recursive type variable, it cannot be instantiated"
                 Ident.(!!(x.descr))
      end

    | `From (x, (y, args)) ->
      let new_r = ref (`Expr dummy_expr) in
      inline_global collect params env y args new_r  (Some x)
    | `Expr _  -> r
  and inline_global collect params env x args new_r teopt =
    let args = List.map (loop collect params env) args in
    let decl = Env.find x global in
    let nparams =
      try
        List.fold_left2 (fun acc x arg ->
            Env.add x arg acc
          ) Env.empty
          decl.decl.params args
      with Invalid_argument _ ->
        let num_params = List.length decl.decl.params in
        error ~loc:x.loc "Parametric type %s expects %d argument%s but was applied to %d"
          Ident.(!!(x.descr))
          num_params
          (if num_params < 2 then "" else "s")
          (List.length args)
    in
    let te = match teopt with
        None -> decl.decl.expr
      | Some n -> Env.find n decl.recs
    in
    let d = loop false nparams env te in
    new_r := `Expr d;
    new_r
  in
  let res = loop true Env.empty Env.empty te in
  res, !recs

module Memo = Hashtbl.Make (struct
    type t = Ast.Located.node ref
    let hash = Hashtbl.hash
    let equal a b = a == b
  end)
let expand te =
  let open Loc in
  let memo = Memo.create 16 in
  let rec loop (te : Ast.Located.typ) =
    with_loc te.loc @@
    match te.descr with
    | (`Typ _ | `Pair _ | `Arrow _ | `Var _ | `Regexp _) as d -> d
    | `Cup l -> `Cup (List.map loop l)
    | `Cap l -> `Cap (List.map loop l)
    | `Diff (t1, t2) -> `Diff (loop t1, loop t2)
    | `Neg t -> `Neg (loop t)
    | `Node r -> `Node (follow te.loc r)
    | `Extra _ -> .
  and follow loc (r : Ast.Located.node ref) =
    try
      let visiting = Memo.find memo r in
      if visiting then
        error ~loc "Ill-founded recursion"
      else r
    with Not_found ->
      Memo.add memo r true;
      begin
        match !r with
          `Expr { descr = `Node r' ; loc } ->
          let r' = follow loc r' in
          r := !r'
        | `Expr te -> r := `Expr (loop te)
        | _ -> assert false
      end;
      Memo.replace memo r false;
      r
  in loop te

let comb_list op f empty l =
  match l with (* avoid suprious operations with any/empty *)
    [] -> empty
  | [ e ] -> f e
  | e :: l -> List.fold_left (fun acc e -> op acc (f e)) (f e) l

let build_type var_map te =
  let open Stt.Typ in
  let memo = Memo.create 16 in
  let rec loop (te:Ast.Located.typ) =
    match te.Loc.descr with
      `Typ t -> node t
    | `Pair (t1, t2) -> node @@ product (loop t1) (loop t2)
    | `Arrow (t1, t2) -> node @@ arrow (loop t1) (loop t2)
    | `Cup l -> node @@ comb_list cup (fun t -> descr (loop t)) Typ.empty l
    | `Cap l -> node @@ comb_list cap (fun t -> descr (loop t)) Typ.any l
    | `Diff (t1, t2) -> node @@ diff (descr (loop t1)) (descr (loop t2))
    | `Neg t -> node @@ neg (descr (loop t))
    | `Var lident ->
      (try
         IdentMap.find lident.descr var_map
       with Not_found -> error ~loc:te.Loc.loc "Unbound polymorphic variable '%s"
                           Ident.(!!(lident.descr)))
    | `Regexp _ -> assert false
    | `Extra _ -> .
    | `Node r -> begin
        match Memo.find memo r with
        | None -> let n = make () in Memo.replace memo r (Some n); n
        | Some n -> n
        | exception Not_found ->
          Memo.add memo r None;
          let nte =
            match !r with
              `Expr te -> loop te
            | _ -> assert false
          in
          match Memo.find memo r with
            None -> Memo.replace memo r (Some nte); nte
          | Some n ->
            def n (descr nte);n
      end
  in loop te

let type_decl global decl =
  let open Ast in
  let te, recs = derecurse global decl.expr in
  let te = expand te in
  let var_list, var_map = List.fold_left (fun (al, am) x ->
      let s = Ident.(!!(x.Loc.descr)) in
      let v = Var.make s in
      let vt = Stt.Typ.(node @@ var v) in
      (x.Loc.descr, v)::al, IdentMap.add x.Loc.descr vt am
    ) ([], IdentMap.empty) decl.params
  in
  let typ = Stt.Typ.descr @@ build_type var_map te in
  let gd = {
    decl;
    vars = List.rev var_list;
    typ;
    recs;
  }
  in
  gd, Env.add decl.name gd global

