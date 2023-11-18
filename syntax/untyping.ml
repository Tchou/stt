open Stt

type 'a node_info = {
  id : int;
  is_rec : bool;
  mutable rec_name : Ident.t option;
  mutable descr : 'a
}

type basic =
    Int of Int.t
  | Char of Char.t
  | Enum of Enum.t
  | Unit of Unit.t

type ident = Ident.t
type typ = (typ, ident, Var.t, re, basic) Ast.Open.typ node_info
and node = (typ, ident) Ast.Open.node
and re = (typ, re) Ast.Open.re


let mk_node_info : bool -> typ =
  let uid = ref 0 in
  fun is_rec  ->
    let id = !uid in
    incr uid;
    { id; is_rec;
      rec_name = None;
      descr = `Cup []}

let set_node_info ni e =  ni.descr <- e; ni

let is_rec_list = List.exists (fun n -> n.is_rec)

let empty_node_info = mk_node_info false
let cup_l l =
  match l with
    [] -> None
  | [ e ] -> Some e
  | _ -> Some (set_node_info (mk_node_info (is_rec_list l)) (`Cup l))

let cap_l l =
  match l with
    [] -> None
  | [ e ] -> Some e
  | _ -> Some (set_node_info (mk_node_info (is_rec_list l)) (`Cap l))

let binop f n1 n2 =
  set_node_info (mk_node_info (n1.is_rec || n2.is_rec)) (f n1 n2)

let pair_ni n1 n2 = binop (fun a b -> `Pair (a, b)) n1 n2
let arrow_ni n1 n2 = binop (fun a b -> `Arrow (a, b)) n1 n2
let diff n1 n2 = binop (fun a b -> `Diff (a, b)) n1 n2
let var v = set_node_info (mk_node_info false)  (`Var v)
let var_l = List.map var
let pos_neg pl nl acc =
  match cap_l pl, cup_l nl with
    None, None-> acc
  | Some p, None -> p::acc
  | None, Some n -> (set_node_info (mk_node_info n.is_rec) (`Neg n))::acc
  | Some p, Some n -> (diff p n)::acc

let untype t =
  let open Typ in
  let name_counter = ref 0 in
  let name_suffix = ref "" in
  let mk_rec_name () =
    let n = Ident.cons ("X" ^ !name_suffix) in
    incr name_counter;
    name_suffix := string_of_int !name_counter;
    n
  in
  let memo = DescrTable.create 16 in
  let loop_basic (type l) (module M : Basic with type Leaf.t = l) mk t acc =
    M.dnf (M.get t) |> Seq.fold_left (fun acc ((pv, nv), a) ->
        let pl = (set_node_info (mk_node_info false) (mk a)) :: var_l pv in
        let nl = var_l nv in
        pos_neg pl nl acc) acc
  in
  let loop_constr (type a) (module M : Constr with type LeafBdd.atom = a) mk t acc =
    M.get t
    |> M.full_dnf
    |> Seq.fold_left (fun acc ((pv, nv), (pa, na)) ->
        let pl = (var_l pv) @ (List.map mk pa) in
        let nl = (var_l nv) @ (List.map mk na) in
        pos_neg pl nl acc) acc
  in
  let rec loop_descr t =
    match DescrTable.find memo t with
      None ->
      let n = mk_node_info true in
      n.rec_name <- Some (mk_rec_name());
      DescrTable.replace memo t (Some n); n
    | Some n -> n
    | exception Not_found ->
      DescrTable.add memo t None;
      let acc = [] in
      let acc = loop_basic (module VarInt) (fun x -> `Extra (Int x)) t acc in
      let acc = loop_basic (module VarChar) (fun x -> `Extra (Char x)) t acc in
      let acc = loop_basic (module VarEnum) (fun x -> `Extra (Enum x)) t acc in
      let acc = loop_basic (module VarUnit) (fun x -> `Extra (Unit x)) t acc in
      let acc = loop_constr (module VarProduct) (loop_node_pair pair_ni) t acc in
      let acc = loop_constr (module VarArrow) (loop_node_pair arrow_ni) t acc in
      let res = match acc with [] -> `Typ Typ.empty | [e] -> e.descr | l -> `Cup l in
      let n = DescrTable.find memo t |> Option.value ~default:(mk_node_info false) in
      n.descr <- res;
      n
  and loop_node_pair mk (n1, n2) =
    let ni1 = loop_descr (descr n1) in
    let ni2 = loop_descr (descr n2) in
    mk ni1 ni2
  in
  let d = loop_descr t in
  match DescrTable.fold (fun _ o acc ->
      match o with
      | Some ({ rec_name = Some n; _ } as r) ->
        (n, r) :: acc
      | _ -> acc
    ) memo []
  with
    [] -> d
  | l ->
    let d = `Node (ref (`Rec (d, l))) in
    let n = mk_node_info false in
    n.descr <- d;
    n