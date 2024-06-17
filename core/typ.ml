open Base

type ('enum, 'int, 'char, 'unit, 'product, 'arrow) descr_ = {
  enum: 'enum;
  int: 'int;
  char: 'char;
  unit: 'unit;
  product: 'product;
  arrow: 'arrow;
}

type 'a node = {
  mutable id: int;
  mutable descr: 'a;
}
module VarEnum = struct
  include Bdd.Make(Var)(Enum)
  let get t = t.enum
  let set enum t = { t with enum }
end

module VarInt = struct
  include Bdd.Make(Var)(Int)
  let get t = t.int
  let set int t = { t with int }
end

module VarChar = struct
  include Bdd.Make(Var)(Char)
  let get t = t.char
  let set char t = { t with char }
end
module VarUnit = struct
  include Bdd.Make(Var)(Unit)
  let get t = t.unit
  let set unit t = { t with unit }
end

module rec Descr:
  Common.T with
  type t = (VarEnum.t, VarInt.t, VarChar.t, VarUnit.t, VarProduct_.t, VarProduct_.t) descr_
= struct
  type t =
    (VarEnum.t, VarInt.t, VarChar.t, VarUnit.t, VarProduct_.t, VarProduct_.t) descr_
  let name = "Typ"

  let equal t1 t2 =
    t1 == t2
    || VarEnum.equal t1.enum t2.enum
       && VarInt.equal t1.int t2.int
       && VarChar.equal t1.char t2.char
       && VarUnit.equal t1.unit t2.unit
       && VarProduct_.equal t1.product t2.product
       && VarProduct_.equal t1.arrow t2.arrow
  let compare t1 t2 =
    let open Base.Common.Let in
    let<> () = VarEnum.compare t1.enum t2.enum in
    let<> () = VarInt.compare t1.int t2.int in
    let<> () = VarChar.compare t1.char t2.char in
    let<> () = VarUnit.compare t1.unit t2.unit in
    let<> () = VarProduct_.compare t1.product t2.product in
    let<> () = VarProduct_.compare t1.arrow t2.arrow in
    0
  let h v x = v + ((x lsl 8) + x)

  let hash t =
    (VarEnum.hash t.enum) |>
    h (VarInt.hash t.int) |>
    h (VarChar.hash t.char) |>
    h (VarUnit.hash t.unit) |>
    h (VarProduct_.hash t.product) |>
    h (VarProduct_.hash t.arrow) |>
    ( land ) max_int

  let pp_one fmt t =
    let open Format in
    fprintf fmt "@[ @[@\n";
    fprintf fmt "@[ATOM    :%a@]@\n" VarEnum.pp (VarEnum.get t);
    fprintf fmt "@[INT     :%a@]@\n" VarInt.pp (VarInt.get t);
    fprintf fmt "@[CHAR    :%a@]@\n" VarChar.pp (VarChar.get t);
    fprintf fmt "@[UNIT    :%a@]@\n" VarUnit.pp (VarUnit.get t);
    fprintf fmt "@[PRODUCT :%a@]@\n" VarProduct_.pp t.product;
    fprintf fmt "@[ARROW   :%a@]@\n" VarProduct_.pp t.arrow;
    fprintf fmt "@[HASH    :%d@]@]@]@\n" (hash t);
  module NodeTable = Hashtbl.Make(Node)
  let rec collect_product memo prod =
    VarProduct_.fold
      ~atom: (fun _ _ _ -> ())
      ~leaf: (
        fun _ p ->
          Product.fold
            ~atom: (
              fun _ () (n1, n2) ->
                collect_node memo n1;
                collect_node memo n2
            )
            ~leaf: (fun () _ -> ())
            ~cup: (fun () () -> ())
            ~empty: ()
            ~any: ()
            p
      )
      ~cup: (fun () () -> ())
      ~empty: ()
      ~any: ()
      prod
  and collect_node memo n =
    if not (NodeTable.mem memo n) then
      begin
        NodeTable.add memo n ();
        collect_descr memo n.descr
      end
  and collect_descr memo t =
    collect_product memo t.product;
    collect_product memo t.arrow

  let pp fmt (t : t) =
    pp_one fmt t;
    let memo = NodeTable.create 16 in
    collect_descr memo t;
    if NodeTable.length memo > 0 then
      begin
        Format.fprintf fmt "@[@ NODES:@[@\n";
        NodeTable.iter
          (
            fun n () ->
              Format.fprintf
                fmt
                "@[%d: %a@]@\n"
                n.id
                pp_one
                n.descr
          )
          memo;
        Format.fprintf fmt "@]@]@\n";
      end
end
and Node: (Common.T with type t = Descr.t node) = struct
  type t = Descr.t node
  let name = "Node"
  let equal (n1 : t) (n2 : t) = n1 == n2
  let compare (n1 : t) (n2 : t) = Stdlib.Int.compare n1.id n2.id
  let hash n = if n.id < 0 then -n.id else n.id
  let pp fmt n = Format.fprintf fmt "@[NODE:%d@]" n.id
end
and Product: (Sigs.Bdd with type atom = Node.t * Node.t) = Bdd.Make(Common.Pair(Node)(Node))(Unit)
and VarProduct_: Sigs.Bdd2 with
  type atom = Var.t
                            and type LeafBdd.atom = Product.atom
                            and type Leaf.t = Product.t
                            and type LeafBdd.t = Product.t
  = Bdd.MakeLevel2(Var)(Product)

module VarProduct = struct
  include VarProduct_
  let get t = t.product
  let set product t = { t with product }
end
module VarArrow = struct
  include VarProduct_
  let get t = t.arrow
  let set arrow t = { t with arrow }
end
include Descr
type descr = t

module type Basic = sig
  include Base.Sigs.Bdd with type atom = Var.t
  val get : descr -> t
  val set : t -> descr -> descr
end

module type Constr = sig
  include Base.Sigs.Bdd2 with type atom = Var.t
  val get : descr -> t
  val set : t -> descr -> descr
end

let empty =
  {
    enum = VarEnum.empty;
    int = VarInt.empty;
    char = VarChar.empty;
    unit = VarUnit.empty;
    product = VarProduct.empty;
    arrow = VarProduct.empty
  }
let any =
  {
    enum = VarEnum.any;
    int = VarInt.any;
    char = VarChar.any;
    unit = VarUnit.any;
    product = VarProduct.any;
    arrow = VarProduct.any
  }

let num_components =
  (* We write it this way to get a compile time error and update
     when we add more fields to descr_ *)
  match { enum = 1; int = 1; char = 1; unit = 1; product = 1; arrow = 1 } with
  | { enum; int; char; unit; product; arrow } ->
    enum + int + char + unit + product + arrow

type component =
    Basic : (module Basic) -> component
  | Constr : (module Basic) * (module Constr) -> component
let all_components =
  [
    Basic (module VarEnum);
    Basic (module VarInt);
    Basic (module VarChar);
    Basic (module VarUnit);
    Constr ((module VarProduct), (module VarProduct));
    Constr ((module VarArrow), (module VarArrow))
  ]

module Singleton = struct
  let enum a = { empty with enum = VarEnum.leaf (Enum.singleton (Base.Hstring.cons a)) }
  let int z = { empty with int = VarInt.leaf (Int.singleton z) }
  let char c = { empty with char = VarChar.leaf (Char.singleton c) }
  let unit = { empty with unit = VarUnit.any }
end

(* Hconsing *)

module DescrTable = Hashtbl.Make(Descr)
module NodeTable = Hashtbl.Make(Node)
let node_memo = DescrTable.create 16
let node_uid = ref 0
let mk_node t =
  incr node_uid;
  { id = !node_uid; descr = t }

let node t =
  try
    DescrTable.find node_memo t
  with
  | Not_found ->
    let n = mk_node t in
    DescrTable.add node_memo t n; n

let descr n =
  assert (n.id >= 0);
  n.descr
let make () =
  let n = mk_node empty in
  n.id <- -n.id;
  n

let def n t =
  assert (n.id < 0);
  n.id <- (-n.id);
  n.descr <- t

let var_product n1 n2 =
  VarProduct.(leaf (Product.atom (n1, n2)))
let product n1 n2 =
  if n1.id > 0 && n1.descr = empty then empty
  else if n2.id > 0 && n2.descr = empty then empty
  else
    { empty with product = var_product n1 n2 }

let arrow n1 n2 =
  { empty with arrow = var_product n1 n2 }

let var v =
  {
    enum = VarEnum.atom v;
    int = VarInt.atom v;
    char = VarChar.atom v;
    unit = VarUnit.atom v;
    product = VarProduct.atom v;
    arrow = VarArrow.atom v
  }

let cup t1 t2 =
  {
    enum = VarEnum.cup t1.enum t2.enum;
    int = VarInt.cup t1.int t2.int;
    char = VarChar.cup t1.char t2.char;
    unit = VarUnit.cup t1.unit t2.unit;
    product = VarProduct.cup t1.product t2.product;
    arrow = VarArrow.cup t1.arrow t2.arrow
  }

let cap t1 t2 =
  {
    enum = VarEnum.cap t1.enum t2.enum;
    int = VarInt.cap t1.int t2.int;
    char = VarChar.cap t1.char t2.char;
    unit = VarUnit.cap t1.unit t2.unit;
    product = VarProduct.cap t1.product t2.product;
    arrow = VarArrow.cap t1.arrow t2.arrow
  }

let diff t1 t2 =
  {
    enum = VarEnum.diff t1.enum t2.enum;
    int = VarInt.diff t1.int t2.int;
    char = VarChar.diff t1.char t2.char;
    unit = VarUnit.diff t1.unit t2.unit;
    product = VarProduct.diff t1.product t2.product;
    arrow = VarArrow.diff t1.arrow t2.arrow
  }

let neg t =
  {
    enum = VarEnum.neg t.enum;
    int = VarInt.neg t.int;
    char = VarChar.neg t.char;
    unit = VarUnit.neg t.unit;
    product = VarProduct.neg t.product;
    arrow = VarArrow.neg t.arrow
  }

type ('var, 'enum, 'int, 'char, 'unit, 'product, 'arrow) op = {
  var: 'var;
  enum: 'enum;
  int: 'int;
  char: 'char;
  unit: 'unit;
  product: 'product;
  arrow: 'arrow;
}

let fold ~op ~cup ~empty ~any t =
  let basic (type l) (module M: Basic with type Leaf.t = l) leaf t =
    M.fold ~atom: op.var ~leaf ~cup ~empty ~any (M.get t)
  in
  let constr (type la) (module M: Constr with type LeafBdd.atom = la) latom t =
    M.fold
      ~atom: op.var
      ~leaf: (
        fun acc l ->
          (M.LeafBdd.fold ~atom: latom ~leaf: (fun acc _ -> acc) ~cup ~empty ~any: acc l)
      )
      ~cup
      ~empty
      ~any
      (M.get t)
  in
  let acc = basic (module VarInt) op.int t in
  let acc = cup acc (basic (module VarChar) op.char t) in
  let acc = cup acc (basic (module VarEnum) op.enum t) in
  let acc = cup acc (basic (module VarUnit) op.unit t) in
  let acc = cup acc (constr (module VarProduct) op.product t) in
  let acc = cup acc (constr (module VarArrow) op.arrow t) in
  acc

let munit f = fun () x -> f x
let munit2 f = fun x () y -> f x y
let ignore2 _ _ = ()

let ignore_iter_op =
  {
    var = ignore2;
    int = ignore;
    char = ignore;
    enum = ignore;
    unit = ignore;
    product = ignore2;
    arrow = ignore2
  }

let iter ~op t =
  fold
    ~op: {
      var = (munit2 op.var);
      enum = (munit op.enum);
      int = (munit op.int);
      char = (munit op.char);
      unit = (munit op.unit);
      product = (munit2 op.product);
      arrow = (munit2 op.arrow)
    }
    ~cup: ignore2
    ~empty: ()
    ~any: ()
    t

let id_map_op = { var = var; int = Fun.id; char = Fun.id; enum = Fun.id; unit = Fun.id; product = Fun.id; arrow = Fun.id }
let map ~op t =
  let basic (type l) (module M: Basic with type Leaf.t = l) f t acc =
    M.set (M.map ~atom: (fun v -> M.get (op.var v)) ~leaf: f (M.get t)) acc
  in
  let constr (type la) (module M: Constr with type LeafBdd.atom = la) f t acc =
    M.set
      (
        M.map
          ~atom: (fun v -> M.get (op.var v))
          ~leaf: (
            fun l ->
              M.LeafBdd.map ~atom: (fun a -> M.LeafBdd.atom (f a)) ~leaf: Fun.id l
          )
          (M.get t)
      )
      acc
  in
  empty |>
  basic (module VarEnum) op.enum t |>
  basic (module VarInt) op.int t |>
  basic (module VarChar) op.char t |>
  basic (module VarUnit) op.unit t |>
  constr (module VarProduct) op.product t |>
  constr (module VarArrow) op.arrow t

module BHNode = Hashtbl.Make(Common.Pair(Common.Bool)(Node))

let vars t =
  let co = ref Var.Set.empty in
  let contra = ref Var.Set.empty in
  let memo = BHNode.create 16 in
  let rec loop_descr pol t =
    iter
      ~op: {
        ignore_iter_op with
        var = (
          fun b v ->
            let set = if pol == b then co else contra in
            set := Var.Set.add v !set
        );
        product = (loop_product pol);
        arrow = (loop_arrow pol)
      }
      t
  and loop_product pol b (n1, n2) =
    loop_node (pol == b) n1; loop_node (pol == b) n2
  and loop_arrow pol b (n1, n2) =
    loop_node (pol != b) n1; loop_node (pol == b) n2
  and loop_node pol n =
    let key = (pol, n) in
    if not (BHNode.mem memo key) then
      begin
        BHNode.add memo key ();
        loop_descr pol n.descr
      end
  in
  loop_descr true t;
  !co, !contra

let toplevel_vars t =
  let co = ref Var.Set.empty in
  let contra = ref Var.Set.empty in
  iter
    ~op: {
      ignore_iter_op with
      var = (
        fun b v ->
          let set = if b then co else contra in
          set := Var.Set.add v !set
      )
    }
    t;
  !co, !contra

let ( &&& ) o (x, b) =
  match o with
  | None -> None
  | Some (y, c) -> if Var.equal x y && b == c then o else None

let get (module M: Basic) t = M.single_atom (M.get t)
let single_var t =
  let open Base.Common.Let in
  let& x = get (module VarEnum) t in
  let& x = get (module VarInt) t &&& x in
  let& x = get (module VarChar) t &&& x in
  let& x = get (module VarUnit) t &&& x in
  let& x = get (module VarProduct) t &&& x in
  let& x = get (module VarArrow) t &&& x in
  Some x

(* Subtyping *)
module Witness =
struct
  type const =
      Int of Int.elem
    | Enum of Enum.elem
    | Char of Char.elem
    | Unit of Unit.elem
    | Pair of (t * t)
    | Arrow of descr
  and t = (Var.t list * Var.t list) * const

  let name = "Witness"

  module VarList = Common.List(Var)
  let rec compare_const c1 c2 =
    let open Common.Let in
    match c1, c2 with
    | Int i1, Int i2 -> Z.compare i1 i2
    | Int _, _ -> -1
    | _, Int _ -> 1
    | Enum e1, Enum e2 -> Base.Hstring.compare e1 e2
    | Enum _, _ -> -1
    | _, Enum _ -> 1
    | Char c1, Char c2 -> Uchar.compare c1 c2
    | Char _, _ -> -1
    | _, Char _ -> 1
    | Unit _, Unit _ -> 0
    | Unit _, _ -> -1
    | _, Unit _ -> 1
    | Pair (a1, b1), Pair (a2, b2) ->
      let<> () = compare a1 a2 in
      compare b1 b2
    | Pair _, _ -> -1
    | _, Pair _ -> 1
    | Arrow a1, Arrow a2 -> Descr.compare a1 a2
  and compare ((p1, n1), c1) ((p2, n2), c2) =
    let open Common.Let in
    let<> () = VarList.compare p1 p2 in
    let<> () = VarList.compare n1 n2 in
    compare_const c1 c2

  let equal e1 e2 = e1 == e2 || compare e1 e2 = 0
  let hash c = Hashtbl.hash c

  let rec pp fmt ((p, n), c) =
    let open Format in
    let ppl fmt ~sep vl =
      VarList.pp_ fmt
        ~op:(fun _ () -> ())
        ~cl:(fun _ () -> ())
        ~sep
        vl
    in
    let () =
      match p, n with
        [], [] -> ()
      | _, [] -> fprintf fmt "@[%a@] & " (ppl ~sep:(fun fmt () -> pp_print_string fmt " & ")) p
      | [], _ -> fprintf fmt "@[~(%a)@] & " (ppl ~sep:(fun fmt () -> pp_print_string fmt " | ")) n
      | _ -> fprintf fmt "@[(%a)\\(%a)@] & "
               (ppl ~sep:(fun fmt () -> pp_print_string fmt " & ")) p
               (ppl ~sep:(fun fmt () -> pp_print_string fmt " | ")) n
    in
    match c with
      Int i -> fprintf fmt "@[%a@]" Z.pp_print i
    | Char c -> fprintf fmt "@['%a'@]" Char.pp_char c
    | Enum e -> fprintf fmt "@[%a@]" Enum.pp_enum e
    | Unit _ -> fprintf fmt "@[()@]"
    | Pair (e1, e2) -> fprintf fmt "@[(@[%a@],@ @[%a@])@]" pp e1 pp e2
    | Arrow _ ->fprintf fmt "<fun>"
end
type elem = Witness.t

exception Found_non_empty of Witness.t

type status = Empty | Non_empty of Witness.t

let memo_subtype = DescrTable.create 16

let non_empty t elem =
  DescrTable.replace memo_subtype t (Non_empty elem);
  raise (Found_non_empty (elem))

let leaf_union (type l) (module M: Basic with type Leaf.t = l) check_leaf t  =
  M.get t
  |> M.dnf
  |> Seq.iter (fun (vars, l) ->
      try
        check_leaf l
      with
        Found_non_empty e ->
        match e with
          ([], []), c -> non_empty t (vars, c)
        | _ -> assert false)

let check_non_empty_basic (type e l) (module M: Basic with type Leaf.t = l and type Leaf.elem = e) (mk : e -> Witness.const) t =
  M.dnf (M.get t) |> Seq.iter (fun (vars, l) ->
      match M.Leaf.sample l with
        Some e -> raise_notrace (Found_non_empty (vars, mk e))
      | None -> ())

let rec check_non_empty stack t =
  match DescrTable.find memo_subtype t with
  | Non_empty e -> non_empty t e
  | Empty -> ()
  | exception Not_found ->
    let new_stack = Base.Dll.push t stack in
    try
      DescrTable.add memo_subtype t Empty;
      let open Witness in
      check_non_empty_basic (module VarEnum) (fun x -> Enum x) t;
      check_non_empty_basic (module VarInt) (fun x -> Int x) t;
      check_non_empty_basic (module VarChar) (fun x -> Char x) t;
      check_non_empty_basic (module VarUnit) (fun x -> Unit x)t;
      leaf_union (module VarProduct) (check_product_conj new_stack) t;
      leaf_union (module VarArrow) (check_arrow_conj new_stack t) t;
      Base.Dll.cut_above new_stack;
      ignore (Base.Dll.pop new_stack);
    with
    | Found_non_empty e ->
      Base.Dll.invalidate_above
        (
          fun t ->
            match DescrTable.find memo_subtype t with
            | Empty -> DescrTable.remove memo_subtype t
            | Non_empty _ -> ()
        )
        new_stack;
      ignore (Base.Dll.pop new_stack);
      non_empty t e

and check_product_conj stack prod_bdd =
  Product.fold
    ~atom: (
      fun b ((t1, t2) as t, l) ((s1, s2) as s) ->
        if b then (cap t1 (descr s1), cap t2 (descr s2)), l
        else (t, s :: l)
    )
    ~leaf: (fun acc _ -> acc)
    ~cup: (fun () ((t1, t2), nprod) -> check_single_prod stack t1 t2 nprod)
    ~any: ((any, any), [])
    ~empty: ()
    prod_bdd

and check_single_prod stack t1 t2 nprod =
  try
    check_non_empty stack t1
  with
  | Found_non_empty w1 ->
    try
      check_non_empty stack t2
    with
    | Found_non_empty w2 ->
      match nprod with
      | [] -> non_empty (product (node t1) (node t2)) (([],[]), Pair(w1, w2))
      | (n1, n2) :: nnprod ->
        check_single_prod stack (diff t1 (descr n1)) t2 nnprod;
        check_single_prod stack t1 (diff t2 (descr n2)) nnprod
and check_arrow_conj stack t arrow_bdd =
  arrow_bdd |>
  Product.dnf |>
  Seq.iter (fun ((pos, neg), _) -> check_neg_arrows stack t pos neg)
and check_neg_arrows stack t pos neg =
  match neg with
  | [] -> non_empty t (([],[]), Arrow t)
  | arr :: nneg ->
    try
      check_single_neg_arrow stack t arr pos
    with
    | Found_non_empty _ -> check_neg_arrows stack t pos nneg
and check_single_neg_arrow stack t (n1, n2) pos =
  let rec loop acc_t1 acc_t2 pos =
    match pos with
    | [] -> non_empty t (([], []), Arrow t)
    | (t1, t2) :: lpos ->
      let acc_t1' = diff acc_t1 (descr t1) in
      try
        check_non_empty stack acc_t1
      with
      | Found_non_empty _ ->
        begin
          loop acc_t1' acc_t2 lpos;
          let acc_t2' = cap acc_t2 (descr t2) in
          try
            check_non_empty stack acc_t2
          with
          | Found_non_empty _ ->
            loop acc_t1 acc_t2' lpos
        end
  in
  loop
    (descr n1)
    (neg (descr n2))
    (match pos with [] -> [(node empty, node any)] | _ -> pos)

let sample t =
  try
    check_non_empty Base.Dll.empty t; None
  with
    Found_non_empty e -> Some e

let is_empty t = Option.is_none (sample t)

let subtype s t = is_empty (diff s t)

let equiv s t = subtype s t && subtype t s


let rec singleton ((pos, neg), c) =
  let open Witness in
  let t = match c with
      Int i -> VarInt.(set (leaf @@ Leaf.singleton i)) empty
    | Enum e -> VarEnum.(set (leaf @@ Leaf.singleton e)) empty
    | Char c -> VarChar.(set (leaf @@ Leaf.singleton c)) empty
    | Unit _ -> VarUnit.set (VarUnit.any) empty
    | Pair (e1, e2) ->
      let t1 = singleton e1 in
      let t2 = singleton e2 in
      product (node t1) (node t2)
    | Arrow t -> t
  in
  List.fold_left (fun acc v ->
      cap acc (var v))
    (List.fold_left (fun acc v ->
         diff acc (var v)) t neg  ) pos

let is_any t = subtype any t

let intersect t1 t2 = not (is_empty (cap t1 t2))

let mem e t =
  let te = singleton e in
  subtype te t
