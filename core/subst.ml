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
      Format.eprintf "%d\n%!" @@ Typ.Node.hash n ;
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
    Typ.map ~op:{ Typ.id_map_op with var; product; arrow = product } t
  in
  let res = loop_descr t in
  HNode.iter (fun _ (n, d) -> Typ.def n d) memo;
  res

let apply s t =
  Format.eprintf "before apply\n%!" ;
  let res = apply_gen (fun v -> try VMap.find v s with Not_found -> Typ.var v) t
  in
  Format.eprintf "after apply\n%!" ;
  res


let refresh t =
  let subst = ref VMap.empty in
  let f v = try VMap.find v !subst with Not_found ->
    let v = Var.(make ~kind:v.kind v.name) in
    let vt = Typ.var v in
    subst := VMap.add v vt !subst;
    vt
  in
  apply_gen f t,!subst