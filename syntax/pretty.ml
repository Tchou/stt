open Format
open Stt
module Name = Base.Hstring

module Regexp = Tmp.Regexp
type regexp = Regexp.t_ext

type t_descr =
    Printer of (formatter -> unit)
  | Pair of t * t
  | Arrow of t * t
  | Cup of t list (* invariant cup/cap list have length >= 1 *)
  | Cap of t list
  | Diff of t * t
  | Neg of t
  | Regexp of Regexp.t_ext
  (*  | Apply of Name.t * t list *)
  | Rec of t * (Name.t * t) list
and t = { typ : Typ.t ;
          descr  : t_descr }



module Prio : sig
  type level = private int
  val level : t -> level
  val lowest : level
end =
struct
  type level = int
  let lowest = 0
  let level t = match t.descr with
      Printer _ | Pair _ (* | Apply _ *) -> 10
    | Neg _-> 9
    | Cap _ | Diff _ -> 8
    | Cup _ -> 7
    | Arrow _ | Regexp _ -> 6
    | Rec _ -> 5
end

let mk typ descr = { typ; descr }

let var v =
  let typ = Typ.var v in
  mk typ (Printer (dprintf "%a" Var.pp v))

let name_descr v =
  Printer (dprintf "%s" Name.(!!v))

let str_descr v =
  Printer (dprintf "%s" v)


let any = mk Typ.any (Printer (dprintf "Any"))
let empty_ = mk Typ.empty (Printer (dprintf "Empty"))

let is_empty_comp (module M : Typ.Basic) t =
  Typ.(is_empty (M.set (M.get t) empty))

let is_any_comp (module M : Typ.Basic) t =
  let any_comp = M.set (M.get Typ.any) Typ.empty in
  Typ.(subtype any_comp (M.set (M.get t) empty))

let choose_complement t =
  let empty_comps = List.fold_left (fun acc b ->
      let i =
        match b with
          Typ.Basic (module M)
        | Typ.Constr ((module M), _) ->
          if is_any_comp (module M) t then 1 else 0
      in
      acc + i
    ) 0 Typ.all_components
  in
  empty_comps > Typ.num_components / 2


module Vars = Base.Common.Pair (Var.Set) (Var.Set)
module VarTable = Hashtbl.Make (Vars)
module DescrTable = Hashtbl.Make (Typ)
let group_by_vars t =
  let open Typ in
  let var_table = VarTable.create 16 in
  let get table key = try VarTable.find table key with Not_found -> empty in
  let replace table key t = VarTable.replace table key t in
  let update table key t = replace table key (cup t (get table key)) in
  (* Group according to the DNF *)
  List.iter (function Basic (module M) | Constr ((module M), _) ->
      M.get t
      |> M.dnf
      |> Seq.iter (fun ((pos, neg), l) ->
          let key = (Var.Set.of_list pos, Var.Set.of_list neg) in
          update var_table key (M.set (M.leaf l) empty)
        )
    ) all_components;
  var_table

let reduce_variables var_table =
  (* assumes (pos <> neg) *)
  let vdest = VarTable.create 16 in
  let empty_key = Var.Set.(empty, empty) in
  let update_empty t =
    let s = try VarTable.find vdest empty_key with Not_found -> Typ.empty in
    VarTable.replace vdest empty_key Typ.(cup t s)
  in
  let te = try VarTable.find var_table empty_key with Not_found -> Typ.empty in
  VarTable.remove var_table empty_key;
  VarTable.add vdest empty_key te;
  VarTable.iter (fun ((pos, neg) as key) t1 ->
      if not (VarTable.mem vdest key) then begin
        try
          let t2 = VarTable.find vdest (neg, pos) in
          let i12 = Typ.cap t1 t2 in
          if Typ.is_empty i12 then begin
            VarTable.add vdest key t1;
            VarTable.add vdest (neg, pos) t2;
          end else begin
            update_empty i12;
            VarTable.add vdest key Typ.(diff t1 i12);
            VarTable.add vdest (neg, pos) Typ.(diff t2 i12);
          end
        with Not_found -> VarTable.add vdest key t1
      end) var_table;
  let te = VarTable.find vdest empty_key in
  if Typ.is_empty te then VarTable.remove vdest empty_key;
  vdest

let split_variables t =
  let table = group_by_vars t in
  reduce_variables table

let is_any t = Typ.(subtype any t)

let pcap l = match l with
    [] -> assert false
  | [ t ] -> t
  | _ ->
    let typ = List.fold_left (fun acc t -> Typ.cap t.typ acc) Typ.any l in
    mk typ (Cap l)

let pcup l = match l with
  | [] -> empty_
  | [ t ] -> t
  | _ ->
    let typ = List.fold_left (fun acc t -> Typ.cup t.typ acc) Typ.empty l in
    mk typ (Cup l)


let any_node = Typ.(node any)
let any_prod = mk Typ.(product any_node any_node) (Pair (any, any))
let any_arrow = mk Builtins.arrow (str_descr "Arrow")

let neg t =
  mk (Typ.neg t.typ) (Neg t)

let prod t1 t2 =
  mk Typ.(product (node t1.typ) (node t2.typ)) (Pair (t1, t2))

let get_leaf (type t) (module M : Typ.Basic with type Leaf.t = t) t =
  match (M.get t |> M.dnf) () with
    Seq.Nil -> M.Leaf.empty
  | Seq.Cons((([], []), l), _ ) -> l
  | _ -> assert false

let pbasic (module M : Typ.Basic) t acc =
  let l = get_leaf (module M) t in
  if M.Leaf.is_empty l then acc else (mk t (Printer (fun ppf -> M.Leaf.pp ppf l)))::acc

let rec_names = Array.map Name.cons [|"X"; "Y"; "Z"; "T"; "U"; "V"; "W"|]
let decompile t =
  let memo = DescrTable.create 16 in
  let name_id = ref 0 in
  let get_name () =
    let i = !name_id in
    incr name_id;
    let len = Array.length rec_names in
    if i >= len then Name.cons ("X" ^ (string_of_int (i-len)))
    else rec_names.(i)
  in
  let rec pr_descr t =
    let descr = pr_descr_ t in
    mk t descr
  and pr_descr_ t =
    match DescrTable.find memo t with
    | Some (_, _, pname) -> pname
    | None ->
      let n = get_name () in
      let pname = name_descr n in
      DescrTable.replace memo t (Some (n, empty_, pname));
      pname
    | exception Not_found ->
      DescrTable.add memo t None;
      let var_table = split_variables t in
      let acc, has_others  =
        match VarTable.find_opt var_table Var.Set.(empty, empty) with
          Some t -> if is_any t then [any], false else
            begin
              VarTable.remove var_table Var.Set.(empty, empty);
              [ pr_choose_compl t ], true
            end
        | None -> [], true
      in
      let acc = if not has_others then acc else
          VarTable.fold (fun (vpos, vneg) t acc ->
              if Typ.is_empty t then acc else
                let tacc = Var.Set.fold (fun v acc -> (var v) :: acc) vpos [] in
                let tacc = Var.Set.fold (fun v acc -> (neg (var v)) :: acc) vneg tacc in
                let tacc = if is_any t then tacc else (pr_choose_compl t) :: tacc in
                (pcap (List.rev tacc)) :: acc)
            var_table acc
      in
      let res = pcup acc in
      match DescrTable.find memo t with
        None -> DescrTable.remove memo t; res.descr
      | Some (n, _, pname) ->
        DescrTable.replace memo t (Some (n, res, pname)); res.descr
  and pr_node n = pr_descr (Typ.descr n)
  and pr_choose_compl t =
    let do_complement = choose_complement t in
    let t = if do_complement then Typ.neg t else t in
    let res = pr_no_var t in
    match res, do_complement with
      [], false -> empty_
    | [], true -> any
    | l, false -> pcup l
    | l, true -> let cl = pcup l in
      mk Typ.(diff any cl.typ) (Diff (any, cl))
  and pr_no_var t =
    let open Typ in
    let acc = [] in
    let acc = pbasic (module VarEnum) t acc in
    let acc = pbasic (module VarInt) t acc in
    let acc = pbasic (module VarChar) t acc in
    let acc = pbasic (module VarUnit) t acc in
    let acc =
      let tp = cap t @@ product (node any) (node any) in
      if Typ.(is_any tp || is_empty tp) then
        acc
      else
        let any_star =
          let x = make () in
          let p = product (node any) x in
          let c = cup Builtins.nil p in
          let () = def x c in
          c
        in
        let ts = cap tp any_star in
        if is_empty ts then
          (* tp :: acc (?) *)
          acc
        else
          (* Somewhere : diff tp ts :: acc (?) *)
          Regexp (pr_regexp ts) :: acc
    in
    let acc = pr_constr (module VarProduct : Basic with type Leaf.t = Product.t)
        (module Product : Base.Sigs.Bdd with type t = Product.t
                                         and type atom = Product.atom
                                         and type Leaf.t = Product.Leaf.t)
        any_prod pr_product_line t acc
    in
    let acc = pr_constr (module VarArrow : Basic with type Leaf.t = Product.t)
        (module Product : Base.Sigs.Bdd with type t = Product.t
                                         and type atom = Product.atom
                                         and type Leaf.t = Product.Leaf.t)
        any_arrow pr_arrow_line t acc
    in
    acc
  and pr_regexp t =
    (* 

      - Need to use pr_descr (why?)
      - Be aware of variables (how to get rid of/ignore them?)

      Need to rethink about the algo 

    *)
    let open Automaton in
    let auto = empty in
    let state =
      let s = ref ~-1 in
      fun () -> incr s; !s
    in
    let init = state () in
    let finals = ref [] in
    let states = ref [(t, init)] in
    let trans = ref [] in
    let loop t q =
      if Typ.subtype Builtins.nil t then
        finals:= q :: !finals ;
      states := (t, q) :: !states ;
      ()
    in
    let () = loop t init in
    let auto = add_states auto @@ List.map snd !states in
    let auto = add_start auto init in
    let auto = add_ends auto !finals in
    let auto = add_transitions auto !trans in
    Regexp.(simp_to_ext @@ to_regex_my auto)
  and pr_constr (type t a l)
      (module V : Typ.Basic with type Leaf.t = t)
      (module C : Base.Sigs.Bdd with type t = t and type atom = a and type Leaf.t = l)
      any pr_line t acc =
    if is_empty_comp (module V) t then acc
    else if is_any_comp (module V) t then any :: acc
    else
      (*let () = Format.eprintf "Going to ARROW LINE CAUSE: %a is not empty\n%!"
        Typ.pp (V.set (V.get t) Typ.empty) in*)
      let dnf = get_leaf (module V) t in
      C.dnf dnf
      |> Seq.fold_left pr_line acc
  and pr_product_line acc ((posp, negp), _) =
    let posp =
      let open Typ in
      match posp with
        [] -> any_prod
      | [ (n1, n2)] ->
        prod (pr_node n1) (pr_node n2)
      | ( n1, n2) :: ll ->
        let n1, n2 = List.fold_left (fun (t1, t2) (n1, n2) ->
            (cap t1 (descr n1), cap t2 (descr n2))) (descr n1, descr n2) ll
        in  prod (pr_descr n1) (pr_descr n2)
    in
    let negp = List.map (fun (n1, n2) -> prod (pr_node n1) (pr_node n2)) negp in
    let res = match negp with
        [] -> posp
      | l ->
        let cl = pcup l in
        mk Typ.(diff posp.typ cl.typ) (Diff(posp, cl))
    in res :: acc
  and pr_arrow_line acc ((posa, nega),_) =
    (*Format.eprintf "IN ARROW LINE\n%!"; *)
    let arrow (n1, n2) =
      let t = Typ.arrow n1 n2 in
      mk t (Arrow (pr_node n1, pr_node n2))
    in
    let posa = List.map arrow posa in
    let nega = List.map arrow nega in
    let posa = match posa with [] -> any_arrow | l -> pcap l in
    let res = match nega with
        [] -> posa
      | l ->
        let cl = pcup l in
        mk Typ.(diff posa.typ cl.typ) (Diff(posa, cl))
    in
    res :: acc
  in
  let res = pr_descr t in
  let recs = DescrTable.fold (fun _ x acc ->
      match x with
        None -> acc
      | Some (n, d, _) -> (n, d)::acc
    ) memo []
  in
  match recs with
    [] -> res
  | _ -> mk t (Rec(res, recs))

let global_print_table = DescrTable.create 16

let rec pp fmt t =
  let d = try DescrTable.find global_print_table t with
      Not_found ->
      let d = decompile t in
      DescrTable.add global_print_table t d;
      d
  in
  pr Prio.lowest fmt d ;

and pr ?(assoc=true) parent_level  ppf t =
  let level = Prio.level t in
  let do_parens = level < parent_level ||
                  (level = parent_level && not assoc)
  in
  fprintf ppf "@[";
  if do_parens then fprintf ppf "(";
  let () =
    match t with
      Printer f -> fprintf ppf "%t" f
    | Pair (t1, t2) -> fprintf ppf "(%a,@ %a)" (pr level) t1 (pr level) t2
    | Arrow (t1, t2) -> fprintf ppf "%a@ ->@ %a" (pr ~assoc:false level) t1 (pr level) t2
    | Cup l -> fprintf ppf "@[%a@]" (pr_list_sep ~sep:" |" level) l
    | Cap l -> fprintf ppf "@[%a@]" (pr_list_sep ~sep:" &" level) l
    | Diff (t1, t2) -> fprintf ppf "%a@ \\@ %a" (pr level) t1 (pr ~assoc:false level) t2
    | Neg t -> fprintf ppf "~%a" (pr level) t
    | Regexp r -> fprintf ppf "%s" @@ Regexp.pp pp r
    (*
    | Apply (n, args) -> fprintf ppf "%s (@[%a@])" Name.(!!n) (pr_list_sep ~sep:"," Prio.lowest) args
    *)
    | Rec (t, defs) -> fprintf ppf "%a@ where@ @[%a@]" (pr level) t pr_defs defs
  in
  if do_parens then fprintf ppf ")";
  fprintf ppf "@]"

and pr_list_sep ~sep level ppf l =
  match l with
    [] -> assert false
  | t :: [] -> pr level ppf t
  | t :: tl -> fprintf ppf "%a%s@ " (pr level) t sep;
    pr_list_sep ~sep level ppf tl

and pr_def ppf (x, t) =
  fprintf ppf "@[%s =@ %a@]" Name.(!!x) (pr Prio.lowest) t
and pr_defs ppf l =
  match l with
    [] -> assert false
  | d :: [] -> pr_def ppf d
  | d :: ll -> fprintf ppf "%a@ and@ " pr_def d;
    pr_defs ppf ll