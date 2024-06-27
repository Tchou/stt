open Format
open Stt
module Name = Base.Hstring

type t_descr =
    Printer of (formatter -> unit)
  | Pair of t * t
  | Arrow of t * t
  | Cup of t list (* invariant cup/cap list have length >= 1 *)
  | Cap of t list
  | Diff of t * t
  | Neg of t
  (*  | Apply of Name.t * t list *)
  | Rec of t * (Name.t * t) list
and t = {
  typ : Typ.t ;
  descr : t_descr
}

let level t =
  match t with
  | Printer _ -> Prio.pr_printer
  | Pair _  -> Prio.pr_pair
  (* | Apply _ -> Prio.pr_apply*)
  | Neg _-> Prio.pr_neg
  | Cap _ -> Prio.pr_cap
  | Diff _ -> Prio.pr_diff
  | Cup _ -> Prio.pr_cup
  | Arrow _ -> Prio.pr_arrow
  | Rec _ -> Prio.pr_rec

let rec pr ?(assoc=true) parent_level ppf t =
  let level = level t in
  let do_parens = level < parent_level ||
                  (level = parent_level && not assoc)
  in
  fprintf ppf "@[";
  if do_parens then fprintf ppf "(";
  let () =
    match t with
      Printer f -> fprintf ppf "%t" f
    | Pair (t1, t2) -> fprintf ppf "(%a,@ %a)" (pr level) t1.descr (pr level) t2.descr
    | Arrow (t1, t2) -> fprintf ppf "%a@ ->@ %a" (pr ~assoc:false level) t1.descr (pr level) t2.descr
    | Cup l -> fprintf ppf "@[%a@]" (pr_list_sep ~sep:" |" level) l
    | Cap l -> fprintf ppf "@[%a@]" (pr_list_sep ~sep:" &" level) l
    | Diff (t1, t2) -> fprintf ppf "%a@ \\@ %a" (pr level) t1.descr (pr ~assoc:false level) t2.descr
    | Neg t -> fprintf ppf "~%a" (pr level) t.descr
    (*
    | Apply (n, args) -> fprintf ppf "%s (@[%a@])" Name.(!!n) (pr_list_sep ~sep:"," Prio.lowest) args
    *)
    | Rec (t, defs) -> fprintf ppf "%a@ where@ @[%a@]" (pr level) t.descr pr_defs defs
  in
  if do_parens then fprintf ppf ")";
  fprintf ppf "@]"

and pr_list_sep ~sep level ppf l =
  match l with
    [] -> assert false
  | t :: [] -> pr level ppf t.descr
  | t :: tl -> fprintf ppf "%a%s@ " (pr level) t.descr sep;
    pr_list_sep ~sep level ppf tl

and pr_def ppf (x, t) =
  fprintf ppf "@[%s =@ %a@]" Name.(!!x) (pr Prio.lowest) t.descr
and pr_defs ppf l =
  match l with
    [] -> assert false
  | d :: [] -> pr_def ppf d
  | d :: ll -> fprintf ppf "%a@ and@ " pr_def d;
    pr_defs ppf ll

let mk typ descr = { typ ; descr }

let var v =
  let typ = Typ.var v in
  mk typ @@ Printer (dprintf "%a" Var.pp v)

let name_descr v =
  Printer (dprintf "%s" Name.(!!v))
let str_descr v =
  Printer (dprintf "%s" v)

let any = mk Typ.any @@ Printer (dprintf "Any")
let empty_ = mk Typ.empty @@ Printer (dprintf "Empty")

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

let neg t =
  mk (Typ.neg t.typ) @@ Neg t
let prod t1 t2 =
  mk Typ.(product (node t1.typ) @@ node t2.typ) @@ Pair (t1, t2)
let diff t1 t2 =
  mk Typ.(diff t1.typ t2.typ) @@ Diff (t1, t2)


let any_node = Typ.(node any)
let any_prod_typ = Typ.(product any_node any_node)
let any_prod = prod any any

let any_arrow = mk Builtins.arrow @@ str_descr "Arrow"

let any_star =
  let open Typ in
  let x = make () in
  let p = product any_node x in
  let c = cup Builtins.nil p in
  let () = def x c in
  c

module Normal = Base.Cartesian.Make (Typ) (Typ)
let extract (n1, n2) = Typ.descr n1, Typ.descr n2

let get_leaf (type t) (module M : Typ.Basic with type Leaf.t = t) t =
  match (M.get t |> M.dnf) () with
    Seq.Nil -> M.Leaf.empty
  | Seq.Cons((([], []), l), _ ) -> l
  | _ -> assert false

let pr_basic (type a) (module M : Typ.Basic with type Leaf.t = a)
    (module E : Base.Sigs.Printable with type t = a) t acc =
  let l = get_leaf (module M) t in
  if M.Leaf.is_empty l then
    acc
  else if M.Leaf.is_any l then
    M.(mk (set (get t) Typ.empty) @@ str_descr Leaf.name) :: acc
  else
    let open Base in
    let export_to_t (t, sg : M.Leaf.t * Pr_basic.single) : t =
      let f fmt : unit = Pr_basic.pp_single fmt sg in
      mk M.(set (leaf t) Typ.empty) @@ Printer f
    in
    let is_diff, union = E.export l in
    let t =
      if is_diff then
        let t1 = M.(mk (set (get t) Typ.empty) @@ str_descr Leaf.name) in
        let t2 = pcup @@ List.map export_to_t union in
        diff t1 t2
      else
        pcup @@ List.map export_to_t union
    in
    t :: acc

let pr_enum t acc =
  let open Typ in
  let l = get_leaf (module VarEnum) t in
  let t, acc =
    if subtype Builtins.bool t && Enum.is_finite l then
      diff t Builtins.bool, (mk Builtins.bool @@ str_descr "Bool") :: acc
    else
      t, acc
  in
  pr_basic (module VarEnum) (module Enum) t acc


let rec_names = Array.map Name.cons [|"X"; "Y"; "Z"; "T"; "U"; "V"; "W"|]

let decompile t =
  let module Lt = struct

    type nonrec t = t

    let compare (t1 : t)
        (t2 : t) : int =
      Typ.compare t1.typ t2.typ

    let pp (fmt : Format.formatter)
        (t : t) : unit = pr Prio.lowest fmt t.descr

    let epsilon = {
      typ = Typ.empty ;
      descr = Printer (fun (_ : formatter) -> ())
    }
    let is_epsilon (t : t) = Typ.is_empty t.typ

    let prio t = level t.descr

  end
  in
  let module Automaton = Automaton.Make(Lt) in
  let module Regexp = Automaton.R
  in
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
    mk t @@ pr_descr_ t
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
    | l, true -> diff any @@ pcup l
  and pr_no_var t =
    let open Typ in
    let acc = [] in
    let t, acc =
      let tp = cap t any_prod_typ in
      if Typ.(equiv tp any_prod_typ || is_empty tp) then
        t, acc
      else
        let ts = cap t any_star in
        if is_empty ts then
          t, acc
        else
          let ts', re = pr_regexp ts in
          let acc = (
            match re with
            | None -> acc
            | Some re -> re :: acc
          )
          in
          cup (diff t ts) ts', acc
    in
    let acc = pr_enum t acc in (* handles the Bool case *)
    let acc = pr_basic (module VarInt) (module Int) t acc in
    let acc = pr_basic (module VarChar) (module Char) t acc in
    let acc = pr_basic (module VarUnit) (module Unit) t acc in
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
  and pr_regexp (t : Typ.t) : Typ.t * t option =
    try
      let open Automaton in
      let auto = create () in
      let memo = Hashtbl.create 16 in
      let rec loop (t : Typ.t)
          (q : state) : unit =
        let () =
          if Typ.subtype Builtins.nil t then
            set_final auto q ;
          let vp, vn = Typ.toplevel_vars t in
          if not Var.Set.(is_empty vp && is_empty vn) then
            raise Exit ;
        in
        let prod = Typ.VarProduct.(full_dnf @@ get t) in
        let prod = Seq.map (
            fun ((vp, vn), (pl, nl)) ->
              match vp, vn with
              | [], [] -> (
                  List.map extract pl,
                  List.map extract nl
                )
              | _ -> raise Exit (* toplevel variables *)
          )
            prod
        in
        let todo =
          Normal.normal prod |>
          List.fold_left (
            fun (acc : (Typ.t * state) list)
              (li, ri : Typ.t * Typ.t) : (Typ.t * state) list ->
              let q', acc = (
                match Hashtbl.find_opt memo ri with
                | None ->
                  let q' = mk_state auto in
                  let () = Hashtbl.add memo ri q' in
                  q', (ri, q') :: acc
                | Some q' -> q', acc
              )
              in
              let () = add_trans auto q (pr_descr li) q' in
              acc
          )
            []
        in
        List.iter (fun (ri, q') -> loop ri q') todo
      in
      let init = mk_state auto in
      let () =
        Hashtbl.add memo t init ;
        loop t init ;
        set_start auto init ;
      in
      let regexp = Regexp.(simplify
                           @@ simp_to_ext
                           @@ to_regex_my auto
                          )
      in
      let printer fmt =
        let pp_lt =
          fun (fmt : formatter)
            (lt : lt) : unit ->
            pr Prio.lowest fmt lt.descr
        in
        Regexp.pp fmt pp_lt regexp
      in
      Typ.empty, Some (mk t @@ Printer printer)
    with Exit ->
      t, None
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
      | [ (n1, n2)] -> prod (pr_node n1) @@ pr_node n2
      | (n1, n2) :: ll ->
        let n1, n2 = List.fold_left (fun (t1, t2) (n1, n2) ->
            (cap t1 (descr n1), cap t2 (descr n2))) (descr n1, descr n2) ll
        in
        prod (pr_descr n1) @@ pr_descr n2
    in
    let negp = List.map (fun (n1, n2) -> prod (pr_node n1) @@ pr_node n2) negp
    in
    let res =
      match negp with
        [] -> posp
      | l -> diff posp @@ pcup l
    in res :: acc
  and pr_arrow_line acc ((posa, nega),_) =
    (*Format.eprintf "IN ARROW LINE\n%!"; *)
    let arrow (n1, n2) =
      let t = Typ.arrow n1 n2 in
      mk t @@ Arrow (pr_node n1, pr_node n2)
    in
    let posa = List.map arrow posa in
    let nega = List.map arrow nega in
    let posa = match posa with [] -> any_arrow | l -> pcap l in
    let res = match nega with
        [] -> posa
      | l -> diff posa @@ pcup l
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
    [] -> res.descr
  | _ -> Rec(res, recs)

let global_print_table = DescrTable.create 16

let pp fmt t =
  let d = try DescrTable.find global_print_table t with
      Not_found ->
      let d = decompile t in
      DescrTable.add global_print_table t d;
      d
  in
  pr Prio.lowest fmt d