module Make (X1 : Sigs.Set) (X2 : Sigs.Set) =
struct
  type simple = (X1.t * X2.t) list Seq.t
  type normal = (X1.t * X2.t) list
  type dnf = ((X1.t * X2.t) list * (X1.t * X2.t) list) Seq.t

  (*
    Eliminate negation by doing :
    (S1xS2)∖(T1xT2) = (S1x(S2\T2)) U ((S1\T1)xS2)
    to remove a single product from S1xS2
    This yield two positive products (at most)
    (R U S)\ T = (R \ T) U (S \ T)

  *)

  let rec single_diff s1 s2 neg acc =
    match neg with
      [] -> (s1, s2)::acc
    | (t1, t2) :: nneg ->
      let d1 = X1.diff s1 t1 in
      let empty_d1 = X1.is_empty d1 in
      let d2 = X2.diff s2 t2 in
      let empty_d2 = X2.is_empty d2 in
      if empty_d1 && empty_d2 then acc
      else if empty_d1 then single_diff s1 d2 nneg acc
      else if empty_d2 then single_diff d1 s2 nneg acc
      else
        single_diff d1 s2 nneg (single_diff s1 d2 nneg acc)

  let rec cap_l any1 any2 l =
    match l with
      [] -> (any1, any2)
    | (x1, x2) :: ll ->
      cap_l (X1.cap any1 x1) (X2.cap any2 x2) ll

  let rec simple dnf =
    match dnf () with
      Seq.Nil -> (fun () -> Seq.Nil)
    | Seq.Cons((pos, neg), ss) ->
      fun () ->
        let s1, s2 = cap_l X1.any X2.any pos in
        if X1.is_empty s1 || X2.is_empty s2 then simple ss ()
        else
          match single_diff s1 s2 neg [] with
            [] -> simple ss ()
          | l -> Seq.Cons (l, fun () -> simple ss ())

  let rec single_disj a1 a2 l acc =
    match l with
      [] -> (a1, a2)::acc
    | ((b1,b2) as b) :: ll ->
      let x = X1.cap a1 b1 in
      if X1.is_empty x then
        single_disj a1 a2 ll (b :: acc)
      else
        let a1' = X1.diff a1 b1 in
        let b1' = X1.diff b1 a1 in
        let empty_a1' = X1.is_empty a1' in
        let empty_b1' = X1.is_empty b1' in
        if empty_a1' && empty_b1' then
          (* a1 = b1 *)
          single_disj a1 (X2.cup a2 b2) ll acc
        else
        if empty_a1' then
          (b1', b2)::(x, X2.cup a2 b2)::(List.rev_append ll acc)
        else
          single_disj a1' a2 ll (
            if empty_b1' then ((x, X2.cup a2 b2)::acc)
            else ((b1', b2)::(x, X2.cup a2 b2)::acc))

  let normal dnf =
    dnf
    |> simple
    |> Seq.flat_map List.to_seq
    |> Seq.fold_left (fun acc (s1, s2) -> single_disj s1 s2 acc []) []

end