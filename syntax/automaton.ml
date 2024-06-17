module type S = sig

  type lt
  type t

  module R : Regexp.S
  type regexp

  val empty : t

  val add_state : t -> int -> t
  val add_states : t -> int list -> t
  val add_trans : t -> int -> lt-> int -> t
  val add_transitions : t -> (int * lt * int) list -> t
  val add_start : t -> int -> t
  val add_starts : t -> int list -> t
  val add_end : t -> int -> t
  val add_ends : t -> int list -> t
  
  val check_word : t -> lt list -> bool

  val to_regex_my : t -> regexp

end

module Make (Lt : Letter.Letter) : S with type lt = Lt.t 
                                     and module R = Regexp.Make(Lt) 
                                     and type regexp = Regexp.Make(Lt).t_simp = struct

  type lt = Lt.t

  type state = int
  module StateSet = Set.Make(Int)
  type states = StateSet.t

  type trans = state * lt * state
  module Trans = struct

    type t = trans

    let compare (state1, letter, state2 : t) 
                (state1', letter', state2' : t) : int =
      let c1 = compare state1 state1' in
      match c1 with
      | 0 ->
        begin
          let c2 = Lt.compare letter letter' in
          match c2 with
          | 0 -> compare state2 state2'
          | _ -> c2
        end
      | _ -> c1

  end
  module TransSet = Set.Make(Trans)
  type transitions = TransSet.t

  type t = { 
              states : states ; 
              starts : states ; 
              trans : transitions ; 
              ends : states ;
           }



  module R = Regexp.Make(Lt)
  type regexp = R.t_simp

  (* ================================================================= *)
  (* ================================================================= *)
  (* ================================================================= *)

  let empty : t = { 
                    states = StateSet.empty ; 
                    starts = StateSet.empty ; 
                    trans = TransSet.empty ;
                    ends = StateSet.empty ; 
                  }



  let add_state (automaton : t) 
                (state : state) : t =
    { 
      automaton with 
        states = StateSet.add state automaton.states 
    }

  let add_states (automaton : t) 
                 (states : state list) : t =
    List.fold_left (
      fun (acc : t) 
          (state : state) : t ->
        add_state acc state
    ) 
    automaton states

  let add_trans (automaton : t) 
                (state1 : state) 
                (letter : lt)
                (state2 : state) : t =
    if StateSet.mem state1 automaton.states && StateSet.mem state2 automaton.states then
      { 
        automaton with 
          trans = TransSet.add (state1, letter, state2) automaton.trans 
      }
    else
      failwith "both given states must be automaton's states"

  let add_transitions (automaton : t) 
                      (transitions : trans list) : t =
    List.fold_left (
      fun (acc : t) 
          (state1, letter, state2 : trans) : t -> 
        add_trans acc state1 letter state2
    ) 
    automaton transitions

  let add_start (automaton : t) 
                (state : state) : t =
    match StateSet.find_opt state automaton.states with
    | Some _ -> { 
        automaton with 
          starts = StateSet.add state automaton.starts 
      }
    | None -> failwith "given state must be an automaton's state"

  let add_starts (automaton : t) 
                 (states : state list) : t =
    List.fold_left (
      fun (acc : t) 
          (state : state) : t -> 
        add_start acc state
    ) 
    automaton states

  let add_end (automaton : t) 
              (state : state) : t =
    match StateSet.find_opt state automaton.states with
    | Some _ -> { 
        automaton with 
          ends = StateSet.add state automaton.ends 
      }
    | None -> failwith "given state must be an automaton's state"

  let add_ends (automaton : t) 
               (states : state list) : t =
    List.fold_left (
      fun (acc : t) 
          (state : state) : t -> 
        add_end acc state
    ) 
    automaton states



  let check_word (automaton : t) 
                 (word : lt list) : bool =
    let end_states =
      List.fold_left (
        fun (current_states : states) 
            (letter : lt) : states ->
          StateSet.fold (
            fun (s : state)
                (next_states : states) : states ->
              let next_states_for_s = TransSet.fold (
                  fun (s1, l, s2 : trans)
                      (next_states_labeled_letter : states) : states ->
                    if s = s1 && Lt.compare letter l = 0 then
                      StateSet.add s2 next_states_labeled_letter
                    else
                      next_states_labeled_letter
                )
                automaton.trans StateSet.empty
              in
              StateSet.union next_states next_states_for_s
          )
          current_states StateSet.empty 
      )
      automaton.starts word
    in
    StateSet.exists (Fun.flip StateSet.mem @@ automaton.ends) end_states



  let to_regex_my (automaton: t) : regexp =
    (* States renaming *)
    let memo = Hashtbl.create 16 in
    let i = ref 1 in
    let () = StateSet.iter (
      fun (s : state) : unit ->
        Hashtbl.add memo s !i ;
        incr i 
    )
    automaton.states
    in
    let replace_states (states : states) : states =
      StateSet.map (
        fun (s : state) : state ->
          Hashtbl.find memo s
      )
      states
    in
    let states = replace_states automaton.states in
    let starts = replace_states automaton.starts in
    let ends = replace_states automaton.ends in
    let trans = TransSet.map (
      fun (s1, l, s2 : trans) : trans ->
        (Hashtbl.find memo s1, l, Hashtbl.find memo s2)
    )
    automaton.trans
    in
    let automaton = { states ; starts ; ends ; trans } in
    (* McNaughton-Yamada algorithm *)
    let n = StateSet.cardinal automaton.states in
    let get_transition_between (trans : transitions) 
                               (state1 : state)
                               (state2 : state) : transitions =
    TransSet.filter (
        fun (state1', _, state2') -> 
          state1 = state1'
          && state2 = state2'
      ) 
      trans
    in
    let mat1 = Array.init n (
      fun (i : state) : regexp array ->
        let line = Array.make n R.empty in
        let () = Array.mapi_inplace (
          fun (j : state) 
              (_ : regexp) : regexp ->
            let transitions = get_transition_between automaton.trans (i+1) (j+1) in
            let regex = TransSet.fold (
              fun (_, l, _ : trans)
                  (acc : regexp) : regexp ->
                R.(
                  if is_empty acc then
                    letter l
                  else
                    union acc @@ letter l
                )
            )
            transitions R.empty
            in
            if TransSet.is_empty transitions then
              if i == j then
                R.letter Lt.epsilon
              else
                regex
            else
              if i == j && not @@ TransSet.mem (i, Lt.epsilon, j) transitions then
                R.(union regex @@ letter Lt.epsilon)
              else
                regex
        ) 
        line 
        in
        line
    )
    in
    let mat2 = Array.make_matrix n n R.empty in
    let choose_mat = ref true in
    let () =
      for k = 0 to n-1 do
        for p = 0 to n-1 do
          for q = 0 to n-1 do
            let algo (mat : regexp array array)
                     (mat' : regexp array array) : unit =
              mat'.(p).(q) <-
                if R.is_empty mat.(p).(k) || R.is_empty mat.(k).(q) then
                  mat.(p).(q)
                else if R.is_empty mat.(p).(q) then
                  if R.is_empty mat.(k).(k) then
                    R.letter Lt.epsilon
                  else
                    R.(
                         concat mat.(p).(k) 
                      @@ concat (star mat.(k).(k)) mat.(k).(q)
                    )
                else if k = p then
                  R.(concat (star mat.(p).(p)) mat.(p).(q))
                else if k = q then
                  R.(
                       concat mat.(p).(q)
                    @@ star mat.(q).(q)
                  )
                else
                  R.(
                       union mat.(p).(q) 
                    @@ concat mat.(p).(k) 
                    @@ concat (star mat.(k).(k)) mat.(k).(q)
                  )
            in
            (* instead of doing copies, i read and write alternately in two matrixes *)
            if !choose_mat then
              algo mat1 mat2
            else
              algo mat2 mat1
          done
        done ;
        choose_mat := not !choose_mat
      done
    in
    StateSet.fold (
      fun (start_state : state)
          (acc : regexp) : regexp ->
        StateSet.fold (
          fun (end_state : state)
              (acc' : regexp) : regexp ->
            if R.is_empty acc' then
              if start_state = end_state then
                R.(
                    union (letter Lt.epsilon) (
                    if !choose_mat then 
                      mat1.(start_state-1).(end_state-1) 
                    else 
                      mat2.(start_state-1).(end_state-1)
                  )
                )
              else
                if !choose_mat then 
                  mat1.(start_state-1).(end_state-1) 
                else 
                  mat2.(start_state-1).(end_state-1)
            else
              if start_state = end_state then
                R.(
                     union acc' 
                  @@ union (letter Lt.epsilon) (
                    if !choose_mat then 
                      mat1.(start_state-1).(end_state-1) 
                    else 
                      mat2.(start_state-1).(end_state-1)
                  )
                )
              else
                R.(
                    union acc' (
                    if !choose_mat then 
                      mat1.(start_state-1).(end_state-1) 
                    else 
                      mat2.(start_state-1).(end_state-1)
                  )
                )
        )
        automaton.ends acc
    )
    automaton.starts R.empty

end