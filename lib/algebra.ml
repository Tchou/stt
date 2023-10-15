open Base

(* From:
   https://github.com/dbuenzli/hmap/blob/master/src/hmap.ml
*)
module Tid = struct
  type _ t = ..
end

module type Tid = sig
  type t
  type _ Tid.t += Tid : t Tid.t
end

type 'a tid = (module Tid with type t = 'a)

let tid () (type s) =
  let module M = struct
    type t = s
    type _ Tid.t += Tid : t Tid.t
  end in
  (module M : Tid with type t = s)

type ('a, 'b) teq = Teq : ('a, 'a) teq

let eq : type r s. r tid -> s tid -> (r, s) teq =
 fun r s ->
  let module R = (val r : Tid with type t = r) in
  let module S = (val s : Tid with type t = s) in
  match R.Tid with S.Tid -> Teq | _ -> assert false

type 'a kind_index = {
  idx : int;
  tid : 'a tid;
  name : string;
}

module type Component = sig
  include Sigs.Set

  val index : t kind_index
end

type map = { apply : 'a. 'a -> (module Component with type t = 'a) -> 'a }
type iter = { iter : 'a. 'a -> (module Component with type t = 'a) -> unit }

type 'b fold = {
  fold : 'a. 'b -> 'a -> (module Component with type t = 'a) -> 'b;
}

let max_component = 16

type mod_pack = Mod : (module Component with type t = 'a) -> mod_pack
type kind_pack = K : 'a kind_index * 'a -> kind_pack

let idx =
  let idx = ref ~-2 in
  (* first index is ~-1 for the dummy key *)
  fun () ->
    incr idx;
    assert (!idx < max_component);
    !idx

let names : (string, int) Hashtbl.t = Hashtbl.create 16

let gen_index ~name =
  if Hashtbl.mem names name then
    failwith ("Name '" ^ name ^ "' is already in use")
  else
    let idx = idx () in
    Hashtbl.add names name idx;
    { idx; tid = tid (); name }

module DummyComponent = struct
  type t = unit
  type elem = unit

  let name = "__DUMMY__"
  let hash () = assert false
  let compare () () = assert false
  let equal () () = assert false
  let empty = ()
  let any = ()
  let is_empty () = assert false
  let is_any () = assert false
  let atom () = assert false
  let cup () () = assert false
  let cap () () = assert false
  let neg () = assert false
  let diff () () = assert false
  let dnf () = assert false
  let index = gen_index ~name
  let pp _ () = assert false
  let singleton () = assert false
  let intersect () () = assert false
  let sample () = assert false
  let mem () () = assert false
end

let modules : mod_pack array =
  Array.make max_component (Mod (module DummyComponent))

let set_module (type a) ((module M) as m : (module Component with type t = a)) =
  Array.unsafe_set modules M.index.idx (Mod m)

let get_module (type a) (index : a kind_index) :
    (module Component with type t = a) =
  let (Mod m) = Array.unsafe_get modules index.idx in
  let module M = (val m) in
  match eq index.tid M.index.tid with Teq -> m

module MakeComponent (K : Sigs.Set) () :
  Component with type t = K.t and type elem = K.elem = struct
  module C = struct
    include K

    let index : t kind_index = gen_index ~name
  end

  let () = set_module (module C)

  include C
end

type t = kind_pack array

let make () = Array.make max_component (K (DummyComponent.index, ()))

let get (type a) tab (index : a kind_index) : a =
  let (K (k, v)) = Array.unsafe_get tab index.idx in
  let Teq = eq k.tid index.tid in
  v

let ( .%() ) tab i = get tab i

let set (type a) tab (index : a kind_index) (v : a) =
  Array.unsafe_set tab index.idx v

let ( .%()<- ) tab i v = set tab i v
let iter f (t : t) = Array.iter (fun (K (k, v)) -> f.iter v (get_module k)) t

let fold f acc (t : t) =
  Array.fold_left (fun acc (K (k, v)) -> f.fold acc v (get_module k)) acc t
