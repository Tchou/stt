type 'a t =
    Nil
  | Cell of { value : 'a ; mutable next : 'a t; mutable prev : 'a t}

let empty = Nil
let is_empty = function Nil -> true | _ -> false

let push v l =
  let c = Cell { value = v; prev = Nil ; next = l} in
  match l with
    Nil -> c
  | Cell r -> r.prev <- c; c


let pop l =
  match l with
  | Cell { value; prev = Nil; next = Nil} ->  value, Nil
  | Cell { value; prev = Nil; next = (Cell n) as next} ->
    n.prev <- Nil; value, next
  | _ -> failwith "pop"

let cut_above l =
  match l with
    Nil -> failwith "cut"
  | Cell { prev = Nil; _ } -> ()
  | Cell ({ prev = Cell p; _ } as r) -> p.next <- Nil; r.prev <- Nil

let rec iter_above f = function
    Nil -> ()
  | Cell { value; prev; _ } -> f value; iter_above f prev

let rec iter_below f = function
    Nil -> ()
  | Cell { value; next; _ } -> f value; iter_below f next


let invalidate_above f c =
  let rec loop = function
      Nil -> ()
    | Cell ({value; prev; _} as r) ->
      f value;
      r.prev <- Nil;
      r.next <- Nil;
      loop prev
  in
  match c with
    Nil -> ()
  | Cell ({ prev; _} as r) -> loop prev; r.prev <- Nil
