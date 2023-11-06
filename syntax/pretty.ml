open Format
open Stt
module Name = Base.Hstring

type t =
    Printer of (formatter -> unit)
  | Pair of t * t
  | Arrow of t * t
  | Cup of t list (* invariant cup/cap list have length >= 1 *)
  | Cap of t list
  | Diff of t * t
  | Neg of t
  (*  | Apply of Name.t * t list *)
  | Rec of t * (Name.t * t) list

module Prio : sig
  type level = private int
  val level : t -> level
  val lowest : level
end =
struct
  type level = int
  let lowest = 0
  let level = function
      Printer _ | Pair _ (* | Apply _ *) -> 10
    | Neg _-> 9
    | Cap _ | Diff _ -> 8
    | Cup _ -> 7
    | Arrow _ -> 6
    | Rec _ -> 5
end

let var v = Printer (dprintf "%a" Var.pp v)
let name v = Printer (dprintf "%s" Name.(!!v))
let any = Printer (dprintf "Any")
let empty_ = Printer (dprintf "Empty")

let rec pr ?(assoc=true) parent_level  ppf t =
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
  (* Merge some entries *)
  let vdest = VarTable.create 16 in
  let entries = VarTable.to_seq var_table |> Array.of_seq in
  let nentries = Array.length entries in
  Format.eprintf "ENTRIES:";
    Array.iter (fun (k, _) -> Format.eprintf "%a; " Vars.pp k) entries;
  Format.eprintf "\n%!";
  for i = 0 to nentries - 1 do
    let ((pos1, neg1) as key1), t1 = entries.(i) in
    for j = i + 1 to nentries - 1 do
      let ((pos2, neg2) as key2), t2 = entries.(j) in
      if Var.Set.equal pos1 neg2 then
        let it12 = cap t1 t2 in
        if not (is_empty it12) then begin
            update vdest (pos2, neg1) it12;
            let nt1 = diff t1 t2 in
            let nt2 = diff t2 t1 in
            entries.(i) <- (key1, nt1);
            entries.(j) <- (key2, nt2);
          end;
    done; 
  done;
  Array.iter (fun (key, t) -> if not (is_empty t) then update vdest key t) entries;
  vdest

module DescrTable = Hashtbl.Make (Typ)
let is_any t = Typ.(subtype any t)
let pcap l = match l with
    [] -> assert false
  | [ t ] -> t
  | _ -> Cap l

let pcup l = match l with
  | [] -> assert false
  | [ t ] -> t
  | _ -> Cup l

let any_prod = Pair (any, any)
let any_arrow = name (Name.cons "Arrow")

let get_leaf (type t) (module M : Typ.Basic with type Leaf.t = t) t =
  match (M.get t |> M.dnf) () with
    Seq.Nil -> M.Leaf.empty
  | Seq.Cons((([], []), l), _ ) -> l
  | _ -> assert false

let pbasic (module M : Typ.Basic) t acc =
  let l = get_leaf (module M) t in
  if M.Leaf.is_empty l then acc else (Printer (fun ppf -> M.Leaf.pp ppf l))::acc

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
    match DescrTable.find memo t with
    | Some (_, _, pname) -> pname
    | None ->
      let n = get_name () in
      let pname = name n in
      DescrTable.replace memo t (Some (n, empty_, pname));
      pname
    | exception Not_found ->
      DescrTable.add memo t None;
      let do_complement = choose_complement t in
      let res =
        let t = if do_complement then Typ.neg t else t in
        let var_table = group_by_vars t in
        match VarTable.find_opt var_table (Var.Set.empty,Var.Set.empty) with
          Some t ->
          if is_any t then [any] else [pr_descr_no_var t]
        | None ->
          let acc = VarTable.fold (fun (vpos, vneg) t acc ->
              let tacc = Var.Set.fold (fun v acc -> (var v) :: acc) vpos [] in
              let tacc = Var.Set.fold (fun v acc -> (Neg (var v)) :: acc) vneg tacc in
              let tacc = if is_any t then tacc else (pr_descr_no_var t) :: tacc in
              (pcap (List.rev tacc)) :: acc)
              var_table []
          in acc
      in
      let res = match res, do_complement with
          [], false -> empty_
        | [], true -> any
        | l, false -> pcup l
        | l, true -> Diff(any, pcup l)
      in
      match DescrTable.find memo t with
        None -> DescrTable.remove memo t; res
      | Some (n, _, pname) ->
        DescrTable.replace memo t (Some (n,res, pname)); res
  and pr_node n = pr_descr (Typ.descr n)
  and pr_descr_no_var t =
    let open Typ in
    let acc = [] in
    let acc = pbasic (module VarEnum) t acc in
    let acc = pbasic (module VarInt) t acc in
    let acc = pbasic (module VarChar) t acc in
    let acc = pbasic (module VarUnit) t acc in
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
    match acc with
      [] -> empty_
    |  l ->    pcup l
  and pr_constr (type t a l)
      (module V : Typ.Basic with type Leaf.t = t)
      (module C : Base.Sigs.Bdd with type t = t and type atom = a and type Leaf.t = l)
      any pr_line t acc =
    if is_empty_comp (module V) t then acc
    else if is_any_comp (module V) t then any :: acc
    else
      let dnf = get_leaf (module V) t in
      C.dnf dnf
      |> Seq.fold_left pr_line acc
  and pr_product_line acc ((posp, negp), _) =
    let posp =
      let open Typ in
      match posp with
        [] -> None
      | [ (n1, n2)] -> Some (Pair(pr_node n1, pr_node n2))
      | ( n1, n2) :: ll ->
        let n1, n2 = List.fold_left (fun (t1, t2) (n1, n2) ->
            (cap t1 (descr n1), cap t2 (descr n2))) (descr n1, descr n2) ll
        in  Some ( Pair (pr_descr n1, pr_descr n2))
    in
    let negp = List.map (fun (n1, n2) -> Pair (pr_node n1, pr_node n2)) negp in
    let posp = match posp with None ->Pair (any, any) | Some l -> l in
    let res = match negp with
        [] -> posp
      | l -> Diff(posp, pcup l)
    in res :: acc
  and pr_arrow_line acc ((posa, nega),_) =
    let arrow (n1, n2) = Arrow (pr_node n1, pr_node n2) in
    let posa = List.map arrow posa in
    let nega = List.map arrow nega in
    let posa = match posa with [] -> any_arrow | l -> pcap l in
    let res = match nega with
        [] -> posa
      | l -> Diff(posa, pcup l)
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