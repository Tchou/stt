module VMap = Map.Make(Var)
type 'a t = 'a VMap.t
let to_list s = s |> VMap.to_seq |> List.of_seq

let add k v m =
  VMap.update k (function None -> Some v | Some _ ->
      invalid_arg (Format.asprintf "Duplicate variable: %a" Var.pp k)
    ) m

let of_list l =
  List.fold_left (fun acc (k, v) -> add k v acc) VMap.empty l


let id_var_basic (type a) (module M : Typ.Basic with type t = a) v : a =
  M.atom v

let find_basic (type a) (module M: Typ.Basic with type t = a) s v =
  try M.get (VMap.find v s) with
  | Not_found -> id_var_basic (module M) v


module HNode = Hashtbl.Make(Typ.Node)
let apply s t =
  let apply_basic (module M : Typ.Basic) t =
    M.set (M.map ~atom:(find_basic (module M) s)
             ~leaf:Fun.id (M.get t)) t
  in
  let apply_constr (type l la)
      (module M : Typ.Constr with type Leaf.t = l
                              and type Leaf.atom = la) (leaf:(la -> l)) t =
    M.set (M.map ~atom:(find_basic (module M) s)
             ~leaf:(fun l ->
                 M.Leaf.map ~atom:leaf
                   ~leaf:Fun.id l
               ) (M.get t)) t
  in
  let memo = HNode.create 16 in
  let rec apply_product (n1, n2) =
    let nn1 = loop_node n1 in
    let nn2 = loop_node n2 in
    Typ.Product.atom (nn1, nn2)
  and loop_descr d =
    let open Typ in
    d
    |> apply_basic (module VarInt)
    |> apply_basic (module VarChar)
    |> apply_basic (module VarAtom)
    |> apply_basic (module VarUnit)
    |> apply_constr (module VarProduct) apply_product
    |> apply_constr (module VarArrow) apply_product

  and loop_node n =
    try fst (HNode.find memo n) with
      Not_found ->
      let nnode = Typ.make () in
      HNode.add memo n (nnode, Typ.empty);
      let res = loop_descr (Typ.descr n) in
      HNode.replace memo n (nnode, res);
      nnode
  in
  let res = loop_descr t in
  HNode.iter (fun _ (n,d) -> Typ.def n d) memo;
  res

let add_var co contra pol v =
  if pol then co := Var.Set.add v !co
  else contra := Var.Set.add v !contra
open Base
module BHNode = Hashtbl.Make(Common.Pair(Common.Bool)(Typ.Node))
let vars t =
  let co = ref Var.Set.empty in
  let contra = ref Var.Set.empty in
  let iter_line f pol (pos, neg) =
    List.iter (f pol) pos;
    List.iter (f (not pol)) neg
  in
  let iter_var_line pol vline =
    iter_line (add_var co contra) pol  vline
  in
  let iter_basic (module M : Typ.Basic) pol t =
    M.get t
    |> M.dnf
    |> Seq.iter (fun (vline,_) -> iter_var_line pol vline)
  in
  let iter_constr (type a) (module M : Typ.Constr with type Leaf.atom = a)
      pol iter_atom t =
    M.get t
    |> M.full_dnf
    |> Seq.iter (fun (vline, atoms) ->
        iter_var_line pol vline;
        iter_atom pol atoms)
  in
  let memo = BHNode.create 16 in
  let rec iter_descr pol d =
    let open Typ in
    let () = iter_basic (module VarInt) pol d in
    let () = iter_basic (module VarChar) pol d in
    let () = iter_basic (module VarAtom) pol d in
    let () = iter_basic (module VarUnit) pol d in
    let () = iter_constr (module VarProduct) pol (iter_line iter_product) d in
    let () = iter_constr (module VarArrow) pol (iter_line iter_arrow) d in
    ()
  and iter_product pol (n1, n2) =
    iter_node pol n1;
    iter_node pol n2
  and iter_arrow pol (n1, n2) =
    iter_node (not pol) n1;
    iter_node pol n2
  and iter_node pol n =
    let key = (pol, n) in
    if not (BHNode.mem memo key) then begin
      BHNode.add memo key ();
      iter_descr pol (Typ.descr n)
    end
  in
  iter_descr true t;
  !co, !contra