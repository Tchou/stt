open Base

type ('enum, 'int, 'char, 'unit, 'product, 'arrow) descr_ = {
  enum : 'enum;
  int : 'int;
  char : 'char;
  unit : 'unit;
  product : 'product;
  arrow : 'arrow;
}

module Get =
struct
  let enum t = t.enum
  let int t = t.int
  let char t = t.char
  let unit t = t.unit
  let product t = t.product
  let arrow t = t.arrow
end
module Set =
struct
  let enum enum t = { t with enum }
  let int int t = { t with int }
  let char char t = { t with char }
  let unit unit t = { t with unit }
  let product product t = { t with product }
  let arrow arrow t   = { t with arrow }
end

type 'a node = {
  mutable id : int;
  mutable descr : 'a;
}
module VarEnum =
struct
  include Bdd.Make (Var) (Enum)
  let get = Get.enum
  let set = Set.enum
end

module VarInt = struct
  include Bdd.Make (Var) (Int)
  let get = Get.int
  let set = Set.int
end

module VarChar = struct
  include Bdd.Make (Var) (Char)
  let get = Get.char
  let set = Set.char
end
module VarUnit = struct
  include Bdd.Make (Var) (Unit)
  let get = Get.unit
  let set = Set.unit
end

module rec Descr :
  Common.T
  with type t =
         ( VarEnum.t,
           VarInt.t,
           VarChar.t,
           VarUnit.t,
           VarProduct_.t,
           VarProduct_.t )
           descr_ = struct
  type t =
    ( VarEnum.t,
      VarInt.t,
      VarChar.t,
      VarUnit.t,
      VarProduct_.t,
      VarProduct_.t )
      descr_
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
    (VarEnum.hash t.enum)
    |> h (VarInt.hash t.int)
    |> h (VarChar.hash t.char)
    |> h (VarUnit.hash t.unit)
    |> h (VarProduct_.hash t.product)
    |> h (VarProduct_.hash t.arrow)
    |> (land) max_int

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
  module Memo = Hashtbl.Make(Node)
  let rec collect_product memo prod =
    VarProduct_.fold
      ~atom:(fun _ _ _ -> ())
      ~leaf:(fun _ p ->
          Product.fold
            ~atom:(fun _ () (n1, n2) ->
                collect_node memo n1;
                collect_node memo n2
              )
            ~leaf:(fun () _ -> ())
            ~cup: (fun () () -> ())
            ~empty:()
            ~any:()
            p
        )
      ~cup: (fun () () -> ())
      ~empty:()
      ~any:()
      prod
  and collect_node memo n =
    if not (Memo.mem memo n) then begin
      Memo.add memo n ();
      collect_descr memo n.descr
    end
  and collect_descr memo t =
    collect_product memo t.product;
    collect_product memo t.arrow

  let pp fmt (t : t) =
    pp_one fmt t;
    let memo = Memo.create 16 in
    collect_descr memo t;
    if Memo.length memo > 0 then begin
      Format.fprintf fmt "@[@ NODES:@[@\n";
      Memo.iter (fun n () ->
          Format.fprintf fmt "@[%d: %a@]@\n" n.id
            pp_one  n.descr
        ) memo;
      Format.fprintf fmt "@]@]@\n";
    end

end

and Node : (Common.T with type t = Descr.t node) = struct
  type t = Descr.t node
  let name = "Node"
  let equal (n1 : t) (n2 : t) = n1 == n2
  let compare (n1 : t) (n2 : t) = Stdlib.Int.compare n1.id n2.id
  let hash n = if n.id < 0 then -n.id else n.id
  let pp fmt n = Format.fprintf fmt "@[NODE:%d@]" n.id
end

and Product : (Sigs.Bdd with type atom = Node.t * Node.t) =
  Bdd.Make (Common.Pair (Node) (Node)) (Unit)

and VarProduct_ : Sigs.Bdd2 with type atom = Var.t
                             and type LeafBdd.atom = Product.atom
                             and type Leaf.t = Product.t
                             and type LeafBdd.t = Product.t

  = Bdd.MakeLevel2 (Var) (Product)

module VarProduct =
struct
  include VarProduct_
  let get = Get.product
  let set = Set.product
end
module VarArrow =
struct
  include VarProduct_
  let get = Get.arrow
  let set = Set.arrow
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

let empty = {
  enum = VarEnum.empty;
  int = VarInt.empty;
  char = VarChar.empty;
  unit = VarUnit.empty;
  product = VarProduct.empty;
  arrow = VarProduct.empty
}
let any = { enum = VarEnum.any;
            int = VarInt.any;
            char = VarChar.any;
            unit = VarUnit.any;
            product = VarProduct.any;
            arrow = VarProduct.any}


let num_components =
  (* We write it this way to get a compile time error and update
     when we add more fields to descr_ *)
  match {enum=1;int=1;char=1;unit=1;product=1;arrow=1} with
    { enum; int; char; unit; product; arrow} ->
    enum+int+char+unit+product+arrow

type component =
    Basic : (module Basic) -> component
  | Constr : (module Basic) * (module Constr) -> component
let all_components =
  [ Basic (module VarEnum); Basic (module VarInt);
    Basic (module VarChar); Basic (module VarUnit);
    Constr ((module VarProduct), (module VarProduct));
    Constr ((module VarArrow), (module VarArrow))]

module Singleton =
struct
  let enum a = {empty with enum = VarEnum.leaf (Enum.singleton (Base.Hstring.cons a)) }
  let int z = { empty with int = VarInt.leaf (Int.singleton z) }
  let char c = { empty with char = VarChar.leaf (Char.singleton c)}
  let unit = { empty with unit = VarUnit.any }

end

(* Hconsing *)

module DescrTable = Hashtbl.Make (
  struct
    include Descr
    let equal a b =
      let r = equal a b in
      if (not r) && hash a = hash b then
        Format.eprintf "Collision: %d %a %a@\n%!" (hash a) pp a pp b;
      r
  end)
let node_memo = DescrTable.create 16
let node_uid = ref 0
let mk_node t =
  incr node_uid;
  { id = !node_uid; descr = t }

let node t =
  try
    DescrTable.find node_memo t
  with
    Not_found ->
    let n = mk_node t in
    DescrTable.add node_memo t n; n

let descr n =
  assert (n.id >= 0);
  n.descr
let make () =
  let n = mk_node empty in
  n.id <- - n.id;
  n

let def n t =
  if n.id >= 0 then begin
    Format.eprintf "Setting %a in %a, but already contains %a@\n"
      Descr.pp t
      Node.pp n
      Descr.pp n.descr
  end;
  assert (n.id < 0);
  n.id <- (-n.id);
  n.descr <- t

let var_product n1 n2 =
  VarProduct.(leaf (Product.atom (n1, n2)))
let product n1 n2 =
  if n1.id > 0 && n1.descr = empty then empty
  else if n2.id > 0 && n2.descr = empty then empty
  else
    { empty with product = var_product n1 n2}

let arrow n1 n2 =
  { empty with arrow = var_product n1 n2}

let var v = {
  enum = VarEnum.atom v;
  int =VarInt.atom v;
  char = VarChar.atom v;
  unit = VarUnit.atom v;
  product = VarProduct.atom v;
  arrow = VarArrow.atom v
}

let cup t1 t2 = {
  enum = VarEnum.cup t1.enum t2.enum;
  int = VarInt.cup t1.int t2.int;
  char = VarChar.cup t1.char t2.char;
  unit = VarUnit.cup t1.unit t2.unit;
  product = VarProduct.cup t1.product t2.product;
  arrow = VarArrow.cup t1.arrow t2.arrow
}

let cap t1 t2 = {
  enum = VarEnum.cap t1.enum t2.enum;
  int = VarInt.cap t1.int t2.int;
  char = VarChar.cap t1.char t2.char;
  unit = VarUnit.cap t1.unit t2.unit;
  product = VarProduct.cap t1.product t2.product;
  arrow = VarArrow.cap t1.arrow t2.arrow
}

let diff t1 t2 = {
  enum = VarEnum.diff t1.enum t2.enum;
  int = VarInt.diff t1.int t2.int;
  char = VarChar.diff t1.char t2.char;
  unit = VarUnit.diff t1.unit t2.unit;
  product = VarProduct.diff t1.product t2.product;
  arrow = VarArrow.diff t1.arrow t2.arrow
}

let neg t = {
  enum = VarEnum.neg t.enum;
  int = VarInt.neg t.int;
  char = VarChar.neg t.char;
  unit = VarUnit.neg t.unit;
  product = VarProduct.neg t.product;
  arrow = VarArrow.neg t.arrow
}

type ('var, 'enum, 'int, 'char, 'unit, 'product, 'arrow) op =
  {
    var : 'var;
    enum : 'enum;
    int : 'int;
    char : 'char;
    unit : 'unit;
    product : 'product;
    arrow : 'arrow;
  }

let fold ~op ~cup ~empty ~any t =
  let basic (type l) (module M : Basic with type Leaf.t = l) leaf t =
    M.fold ~atom:op.var ~leaf ~cup ~empty ~any (M.get t)
  in
  let constr (type la) (module M : Constr with type LeafBdd.atom = la) latom t =
    M.fold ~atom:op.var ~leaf:(
      fun acc l ->
        (M.LeafBdd.fold ~atom:latom ~leaf:(fun acc _ ->acc) ~cup ~empty ~any:acc l)
    )
      ~cup ~empty ~any (M.get t)
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

let ignore_iter_op = { var = ignore2; int = ignore;
                       char = ignore; enum = ignore; unit = ignore; product = ignore2; arrow = ignore2}

let iter ~op t =
  fold ~op:{var = (munit2 op.var);
            enum = (munit op.enum);
            int = (munit op.int);
            char = (munit op.char);
            unit = (munit op.unit);
            product = (munit2 op.product);
            arrow = (munit2 op.arrow)
           }
    ~cup:ignore2
    ~empty:()
    ~any:()
    t

let id_map_op = { var = var; int=Fun.id; char=Fun.id; enum=Fun.id;unit=Fun.id; product=Fun.id;arrow=Fun.id}
let map ~op t =
  let basic (type l) (module M : Basic with type Leaf.t = l) f t acc =
    M.set (M.map ~atom:(fun v -> M.get (op.var v)) ~leaf:f (M.get t)) acc
  in
  let constr (type la) (module M : Constr with type LeafBdd.atom = la) f t acc =
    M.set (M.map ~atom:(fun v -> M.get (op.var v)) ~leaf:(fun l ->
        M.LeafBdd.map ~atom:(fun a -> M.LeafBdd.atom (f a)) ~leaf:Fun.id l
      ) (M.get t)) acc
  in
  empty
  |> basic (module VarEnum) op.enum t
  |> basic (module VarInt) op.int t
  |> basic (module VarChar) op.char t
  |> basic (module VarUnit) op.unit t
  |> constr (module VarProduct) op.product t
  |> constr (module VarArrow) op.arrow t


module BHNode = Hashtbl.Make(Common.Pair(Common.Bool)(Node))

let vars t =
  let co = ref Var.Set.empty in
  let contra = ref Var.Set.empty in
  let memo = BHNode.create 16 in
  let rec loop_descr pol t =
    iter ~op:{ ignore_iter_op with
               var=(fun b v ->
                   let set = if pol == b then co else contra in
                   set := Var.Set.add v !set);
               product = (loop_product pol);
               arrow = (loop_arrow pol) }
      t
  and loop_product pol b (n1, n2) =
    loop_node (pol == b) n1; loop_node (pol == b) n2
  and loop_arrow pol b (n1, n2) =
    loop_node (pol != b) n1; loop_node (pol == b) n2
  and loop_node pol n =
    let key = (pol, n) in
    if not (BHNode.mem memo key) then begin
      BHNode.add memo key ();
      loop_descr pol n.descr
    end
  in
  loop_descr true t;
  !co, !contra

let toplevel_vars t =
  let co = ref Var.Set.empty in
  let contra = ref Var.Set.empty in
  iter ~op:{ignore_iter_op with
            var=(fun b v ->
                let set = if b then co else contra in
                set := Var.Set.add v !set) }
    t; !co, !contra


let (&&&) o (x, b) =
  match o with
    None -> None
  | Some (y, c) -> if Var.equal x y && b == c then o else None

let get (module M : Basic) t = M.single_atom (M.get t)
let single_var t =
  let open Base.Common.Let in
  let| x = get (module VarEnum) t in
  let| x = get (module VarInt) t &&& x in
  let| x = get (module VarChar) t &&& x in
  let| x = get (module VarUnit) t &&& x in
  let| x = get (module VarProduct) t &&& x in
  let| x = get (module VarArrow) t &&& x in
  Some x

(* Subtyping *)

exception Found_non_empty

type status = Empty | Non_empty

let memo_subtype = DescrTable.create 16

let non_empty t =
  DescrTable.replace memo_subtype t Non_empty;
  raise Found_non_empty

let leaf_union (type l) (module M : Basic with type Leaf.t = l) t : l Seq.t =
  M.fold ~atom:(fun _  () _ -> ())
    ~leaf:(fun () leaf -> leaf)
    ~cup:(fun s l -> fun () -> Seq.Cons(l, s))
    ~empty:(fun () -> Seq.Nil)
    ~any:()
    (M.get t)

let check_non_empty_basic (module M : Basic) t =
  if not (M.is_empty (M.get t)) then raise_notrace Found_non_empty

let rec check_non_empty stack t =
  match DescrTable.find memo_subtype t with
    Non_empty -> non_empty t
  | Empty -> ()
  | exception Not_found ->
    let new_stack = Base.Dll.push t stack in
    try
      DescrTable.add memo_subtype t Empty;
      check_non_empty_basic (module VarEnum) t;
      check_non_empty_basic (module VarInt) t;
      check_non_empty_basic (module VarChar) t;
      check_non_empty_basic (module VarUnit) t;
      leaf_union (module VarProduct) t |> Seq.iter (check_product_conj new_stack);
      leaf_union (module VarArrow) t |> Seq.iter (check_arrow_conj new_stack t);
      Base.Dll.cut_above new_stack;
      ignore (Base.Dll.pop new_stack);
    with  Found_non_empty ->
      Base.Dll.invalidate_above (fun t ->
          match DescrTable.find memo_subtype t with
            Empty -> DescrTable.remove memo_subtype t
          | Non_empty -> ()
        ) new_stack;
      ignore (Base.Dll.pop new_stack);
      non_empty t

and check_product_conj stack prod_bdd =
  Product.fold ~atom:(fun b ((t1, t2) as t,l) ((s1, s2) as s) ->
      if b then (cap t1 (descr s1), cap t2 (descr s2)), l
      else (t, s::l))
    ~leaf:(fun acc _ -> acc)
    ~cup:(fun () ((t1, t2), nprod) -> check_single_prod stack t1 t2 nprod)
    ~any:((any, any), [])
    ~empty:() prod_bdd

and check_single_prod stack t1 t2 nprod =
  try check_non_empty stack t1 with Found_non_empty ->
  try check_non_empty stack t2 with Found_non_empty ->
  match nprod with
    [] -> non_empty (product (node t1) (node t2))
  | (n1, n2) :: nnprod ->
    check_single_prod stack (diff t1 (descr n1)) t2 nnprod;
    check_single_prod stack t1 (diff t2 (descr n2)) nnprod
and check_arrow_conj stack t arrow_bdd =
  arrow_bdd
  |> Product.dnf
  |> Seq.iter (fun ((pos, neg),_) -> check_neg_arrows stack t pos neg)
and check_neg_arrows stack t pos neg =
  match neg with
    [ ] -> non_empty t
  | arr :: nneg ->
    try check_single_neg_arrow stack t arr pos
    with Found_non_empty -> check_neg_arrows stack t pos nneg
and check_single_neg_arrow stack t (n1, n2) pos =
  let rec loop acc_t1 acc_t2 pos =
    match pos with
      [] -> non_empty t
    | (t1, t2) :: lpos ->
      let acc_t1' = diff acc_t1 (descr t1) in
      try check_non_empty stack acc_t1 with Found_non_empty -> begin
          loop acc_t1' acc_t2 lpos;
          let acc_t2' = cap acc_t2 (descr t2) in
          try check_non_empty stack acc_t2 with Found_non_empty ->
            loop acc_t1 acc_t2' lpos
        end
  in
  loop (descr n1) (neg (descr n2)) pos

let is_empty t =
  let res =
    try
      check_non_empty Base.Dll.empty t; true
    with Found_non_empty -> false in
  res

let subtype s t = is_empty (diff s t)

let equiv s t = subtype s t && subtype t s
