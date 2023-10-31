open Base

module VMap = Map.Make (Var)
type 'a t = 'a VMap.t
let to_list s = s |> VMap.to_seq |> List.of_seq

let add k v m =
  VMap.update k (function None -> Some v | Some _ ->
      invalid_arg (Format.asprintf "Duplicate variable: %a" Var.pp k)
    ) m

let of_list l =
  List.fold_left (fun acc (k, v) -> add k v acc) VMap.empty l


let domain subst = VMap.fold (fun v _ acc -> Var.Set.add v acc) subst Var.Set.empty

module HNode = Hashtbl.Make(Typ.Node)

let apply_gen var t =
  let memo = HNode.create 16 in
  let rec product (n1, n2) = (loop_node n1, loop_node n2)
  and loop_node n =
    try fst (HNode.find memo n) with
      Not_found ->
      let nnode = Typ.make () in
      HNode.add memo n (nnode, Typ.empty);
      let d = Typ.descr n in
      let res = loop_descr d in
      if Typ.equal d res then begin
        HNode.remove memo n;
        n
      end else begin
        HNode.replace memo n (nnode, res);
        nnode
      end
  and loop_descr t =
    Typ.map ~var ~atom:Fun.id ~int:Fun.id ~char:Fun.id ~unit:Fun.id ~product ~arrow:product t
  in
  let res = loop_descr t in
  HNode.iter (fun _ (n, d) -> Typ.def n d) memo;
  res

let apply s t =
  apply_gen (fun v -> try VMap.find v s with Not_found -> Typ.var v) t


let refresh t =
  let subst = ref VMap.empty in
  let f v = try VMap.find v !subst with Not_found ->
    let v = Var.(make ~kind:v.kind v.name) in
    let vt = Typ.var v in
    subst := VMap.add v vt !subst;
    vt
  in
  apply_gen f t,!subst


module BHNode = Hashtbl.Make(Common.Pair(Common.Bool)(Typ.Node))

let vars t =
  let co = ref Var.Set.empty in
  let contra = ref Var.Set.empty in
  let memo = BHNode.create 16 in
  let rec loop_descr pol t =
    Typ.iter ~var:(fun b v ->
        let set = if pol == b then co else contra in
        set := Var.Set.add v !set)
      ~int:ignore ~char:ignore
      ~atom:ignore ~unit:ignore
      ~product:(loop_product pol)
      ~arrow:(loop_arrow pol)
      t
  and loop_product pol b (n1, n2) =
    loop_node (pol == b) n1;
    loop_node (pol == b) n2
  and loop_arrow pol b (n1, n2) =
    loop_node (pol != b) n1;
    loop_node (pol == b) n2
  and loop_node pol n =
    let key = (pol, n) in
    if not (BHNode.mem memo key) then begin
      BHNode.add memo key ();
      loop_descr pol (Typ.descr n)
    end
  in
  loop_descr true t;
  !co, !contra

let toplevel_vars t =
  let co = ref Var.Set.empty in
  let contra = ref Var.Set.empty in
  Typ.iter ~var:(fun b v ->
      let set = if b then co else contra in
      set := Var.Set.add v !set)
    ~int:ignore ~char:ignore
    ~atom:ignore ~unit:ignore
    ~product:(fun _ _ -> ())
    ~arrow:(fun _ _ -> ())
    t; !co, !contra