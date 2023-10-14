type t = Algebra.t

let dummy = Algebra.make ()

module Descr = struct
  type nonrec t = t

  let equal t1 t2 =
    if t1 == t2 then true
    else
      try
        Algebra.iter
          {
            iter =
              (fun (type a) (v1 : a)
                   (module M : Algebra.Component with type t = a) ->
                let v2 = Algebra.get t2 M.index in
                if not (M.equal v1 v2) then raise_notrace Exit);
          }
          t1;
        true
      with
      | Exit -> false

  let compare t1 t2 =
    let res = ref 0 in
    let () =
      try
        Algebra.iter
          {
            iter =
              (fun (type a) (v1 : a)
                   (module M : Algebra.Component with type t = a) ->
                let v2 = Algebra.get t2 M.index in
                res := M.compare v1 v2;
                if !res != 0 then raise_notrace Exit);
          }
          t1
      with
      | Exit -> ()
    in
    !res

  let hash t =
    Algebra.fold
      {
        fold =
          (fun (type a) acc (v : a)
               (module M : Algebra.Component with type t = a) ->
            acc lxor 8197 * M.hash v);
      }
      0 t
end

module Node = struct
  type t = {
    id : int;
    mutable descr : Descr.t;
  }

  let equal a b = a == b
  let mk () = { id = Oo.id (object end); descr = dummy }
end
