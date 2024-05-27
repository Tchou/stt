type state = int
module StateSet = Set.Make(Int)
type states = StateSet.t

type trans = state * Stt.Typ.t * state
module Trans = struct

  type t = trans

  let compare (state1, letter, state2 : t) 
              (state1', letter', state2' : t) : int =
    let c1 = compare state1 state1' in
    match c1 with
    | 0 ->
      begin
        let c2 = Stt.Typ.compare letter letter' in
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

(* ================================================================= *)
(* ================================================================= *)
(* ================================================================= *)

let _get_transition_from (trans : transitions) 
                        (state : state) : transitions =
  TransSet.filter (
    fun (state', _, _) -> 
      state = state'
  ) 
  trans

let get_transition_between (trans : transitions) 
                           (state1 : state)
                           (state2 : state) : transitions =
  TransSet.filter (
      fun (state1', _, state2') -> 
        state1 = state1'
        && state2 = state2'
    ) 
    trans

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
  { automaton with states = StateSet.add state automaton.states }

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
              (letter : Stt.Typ.t)
              (state2 : state) : t =
  if StateSet.mem state1 automaton.states && StateSet.mem state2 automaton.states then
    { automaton with trans = TransSet.add (state1, letter, state2) automaton.trans }
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
  | Some _ ->
    { automaton with starts = StateSet.add state automaton.starts }
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
  | Some _ ->
    { automaton with ends = StateSet.add state automaton.ends }
  | None -> failwith "given state must be an automaton's state"

let add_ends (automaton : t) 
             (states : state list) : t =
  List.fold_left (
    fun (acc : t) 
        (state : state) : t -> 
      add_end acc state
  ) 
  automaton states



let determinize (automaton : t) : t = automaton
(*
let determinize (automaton : t) : t =

  module StateSetSet = Set.Make(StateSet)
  type states_set = StateSetSet.t
  module StateSetHash = struct

    type t = states

    let equal : states -> states -> bool = StateSet.equal

    let hash (states : t) : int =
      StateSet.fold (
        fun (state : state) 
            (acc : int) : int ->
          Hashtbl.hash @@ state lxor acc
      ) 
      states 0
     
  end
  module StateSetHashtbl = Hashtbl.Make(StateSetHash)

  (* Only one start state (state nb = min-1)*)
  let start_state = -1 + StateSet.fold min automaton.states 0
  in
  (* Add it as a new state *)
  let automaton = add_state automaton start_state in
  (* Link the new start state with previous start states *)
  let automaton = StateSet.fold (
    fun (state : state)
        (automaton : t) : t ->
      add_trans automaton start_state Stt.Typ.empty state
  ) 
  automaton.starts automaton 
  in
  (* Removes old start states *)
  let automaton = { automaton with starts = StateSet.empty } in
  (* Add new start state *)
  let automaton = add_start automaton start_state 
  in
  (* To get rid of eps transitions
    
     Hashtbl mapping all states the set of states accessible with eps transitions :
     (state, StateSet.t) Hashtbl.t
   *)
  let eps_closure = Hashtbl.create 16 in
  let () = StateSet.iter (
    fun state ->
      let rec get_accessible_states_with_eps_trans (state1 : state) 
                                                   (acc, already_done : states * states) : states =
        (* all transitions *)
        let transitions = get_transition_from automaton.trans state1 in
        (* keep only the eps transitions *)
        let transitions = TransSet.filter (
          fun (_, letter, _ : trans) : bool -> 
            Stt.Typ.is_empty letter
        ) 
        transitions 
        in
        if TransSet.is_empty transitions then
          acc
        else
          let states = TransSet.fold (
            fun (_, _, state2 : trans) 
                (acc : states) : states -> 
              StateSet.add state2 acc
          ) 
          transitions StateSet.empty 
          in
          StateSet.fold (
            fun (state2 : state)
                (acc : states) : states ->
              (* next state already done *)
              if StateSet.mem state2 already_done then
                acc
              (* we call on each next state [state2] and we add all his eps neighbours to [state1]'s ones*)
              else
                StateSet.union acc
                  @@ StateSet.add state2
                  @@ get_accessible_states_with_eps_trans state2
                  @@ (StateSet.empty, StateSet.add state1 already_done)
          ) 
          states StateSet.empty
      in
      (* eps_closure is a map from a state to a StateSet (the set of all the accessible states with eps transitions) *)
      Hashtbl.replace eps_closure state 
        @@ StateSet.add state
        @@ get_accessible_states_with_eps_trans state 
        @@ (StateSet.empty, StateSet.empty)
  ) 
  automaton.states
  in
  (* Merging states 
    
     Hashtbl mapping a set of states to a list of all its transitions from every states in the set
     (states, (Stt.Typ.t * states) list) Hashtbl.t

     expl : {1, 2, 5} -> [ ("a", {1}) ; ("b", {1, 2, 5}) ; ("c", {}) ; ("d", {3, 4}) ], i.e. :
      - From states 1, 2 and 5, with "a", we can get to 1
      - From states 1, 2 and 5, with "b", we can get to 1, 2 or 5 so we add it as a future new state
      - etc.
  *)
  let new_trans = StateSetHashtbl.create 16 in
  (* Starting with the StateSets of the start_state (stack) *)
  let states_to_do = StateSetSet.add (Hashtbl.find eps_closure start_state) StateSetSet.empty in
  let rec merge_transitions (states_to_do : states_set) 
                            (states_done : states_set) : unit =
    if not @@ StateSetSet.is_empty states_to_do then
      let choosen_states = StateSetSet.choose states_to_do in
      let states_to_do = StateSetSet.remove choosen_states states_to_do 
      in
      (* i_transitions : (Stt.Typ.t * states) list 
          = list of all possible next states
        
         n_to_do : states_set 
          = sett of all states we need to apply the algorithm
      *)
      let i_transitions, n_to_do = LetterSet.fold (
        (* For all letter *)
        fun (letter : Stt.Typ.t)
            (i_transitions, stack : (Stt.Typ.t * states) list * states_set) 
              : ((Stt.Typ.t * states) list * states_set) ->
          (* For all states in states, we gather all the next states the accesible states with a transitions labelled letter *)
          let next_states = StateSet.fold (
            fun state1 acc ->
              (* Get all transitions *)
              let transitions = get_transition_from automaton.trans state1 in
              (* All transitions labelled [letter] *)
              let transitions = TransSet.filter (
                fun (_, letter', _ : trans) : bool -> 
                  Stt.Typ.compare letter' letter = 0
              ) 
              transitions 
              in
              (* Sets of all possible next states *)
              let next_states = TransSet.fold (
                fun (_, _, state2 : trans)
                    (acc : states) : states ->
                  StateSet.union acc @@ Hashtbl.find eps_closure state2
              )
              transitions StateSet.empty 
              in
              (* We join with the other accesible states *) 
              StateSet.union acc next_states
          ) 
          choosen_states StateSet.empty 
          in
          let n_stack =
            (* We don't apply the function if
                - there aren't any transitions
                - or we already did the job
                - or it's a loop
             *)
            if StateSet.is_empty next_states || StateSetSet.mem next_states states_done || StateSet.compare next_states choosen_states = 0 then
              stack
            else
              StateSetSet.add next_states stack
          in
          (* We add the transitions labelled [letter] from [choosen_states] to [next_states], 
             and we also return the new to_do_stack *)
          (letter, next_states) :: i_transitions, n_stack
      ) 
      automaton.alphabet ([], states_to_do) 
      in
      let () = StateSetHashtbl.replace new_trans choosen_states i_transitions in
      (* We keep_going the algo with the StateSet in the to_do list, and we add the current states to the done list since we just applied the algo to it*)
      merge_transitions n_to_do @@ StateSetSet.add choosen_states states_done
  in
  let () = merge_transitions states_to_do StateSetSet.empty in
  (* Renaming StateSet into a state 
     This is all the new states
  *)
  let states_name = StateSetHashtbl.create 16 in
  let state_nb = ref 0 in
  let () = StateSetHashtbl.iter (
    fun (states : states) 
        (_ : (Stt.Typ.t * states) list) : unit -> 
      StateSetHashtbl.replace states_name states !state_nb ;
      (* Small counter *)
      state_nb := !state_nb + 1
  ) 
  new_trans 
  in
  (* Gather all new transitions *)
  let transitions = StateSetHashtbl.fold (
    fun (state : states)
        (i_transitions : (Stt.Typ.t * states) list) 
        (acc : transitions) ->
      (* Int name ([state] is a set)*)
      let state1 = StateSetHashtbl.find states_name state in
      (* Add to the acc the new transitions *)
      let transitions = List.fold_left (
        fun (acc' : transitions) 
            (letter, states : Stt.Typ.t * states) : transitions ->
          (* Transitions *)
          if StateSet.is_empty states then
            acc'
          else
            let trans = (state1, letter, StateSetHashtbl.find states_name states) in
            TransSet.add trans acc'
      ) 
      TransSet.empty i_transitions
      in 
      TransSet.union acc transitions
  )
  new_trans TransSet.empty 
  in
  (* Gather all end states *)
  let end_states = StateSetHashtbl.fold (
    fun (states : states) 
        (state_name : state) 
        (acc : states) : states ->
      (* If one of the state in states (which is a StateSet) is an end state, then the state name of states is an end state *)
     if StateSet.exists (fun (state : state) : bool -> StateSet.mem state states) automaton.ends then
        StateSet.add state_name acc
      else
        acc
  ) 
  states_name StateSet.empty
  in
  (* The DFA automaton *)
  { 
    (* Same alphabet *)
    alphabet = automaton.alphabet ; 
    (* Get all new states *)
    states = StateSet.of_seq @@ StateSetHashtbl.to_seq_values states_name ;
    (* Get the name of the state from its "set name" *)
    starts = StateSet.add (StateSetHashtbl.find states_name @@ Hashtbl.find eps_closure start_state) StateSet.empty ; 
    (* Previously calculated transitions *)
    trans = transitions ;
    (* Previously calculated end states *)
    ends = end_states ; 
  }

TODO : Fix algo bc we don't know all the possible letters (types)

*)


let check_word (automaton : t) 
               (word : Stt.Typ.t list) : bool =
  let end_states =
    List.fold_left
    (
      fun (current_states : states) 
          (letter : Stt.Typ.t) : states ->
        StateSet.fold
        (
          fun (s : state)
              (next_states : states) : states ->
            let next_states_for_s = TransSet.fold
              (
                fun (s1, l, s2 : trans)
                    (next_states_labeled_letter : states) : states ->
                  if s = s1 && Stt.Typ.compare letter l = 0 then
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



let to_regex_my (automaton: t) : Regexp.t_simp =
  (* States renaming *)
  let hash = Hashtbl.create 16 in
  let i = ref 1 in
  let () = StateSet.iter (
    fun (s : state) : unit ->
      Hashtbl.add hash s !i ;
      incr i 
  )
  automaton.states
  in
  let replace_states (states : states) : states =
    StateSet.map (
      fun (s : state) : state ->
        Hashtbl.find hash s
    )
    states
  in
  let states = replace_states automaton.states in
  let starts = replace_states automaton.starts in
  let ends = replace_states automaton.ends in
  let trans = TransSet.map (
    fun (s1, l, s2 : trans) : trans ->
      (Hashtbl.find hash s1, l, Hashtbl.find hash s2)
  )
  automaton.trans
  in
  let automaton = { states ; starts ; ends ; trans } in
  (* McNaughton-Yamada algorithm *)
  let n = StateSet.cardinal automaton.states in
  let mat1 = Array.init n (
    fun (i : state) : Regexp.t_simp array ->
      let line = Array.make n Regexp.empty in
      let () = Array.mapi_inplace (
        fun (j : state) 
            (_ : Regexp.t_simp) : Regexp.t_simp ->
          let transitions = get_transition_between automaton.trans (i+1) (j+1) in
          let regex = TransSet.fold (
            fun (_, l, _ : trans)
                (acc : Regexp.t_simp) : Regexp.t_simp ->
              Regexp.(
                if is_empty acc then
                  letter l
                else
                  union acc @@ letter l
              )
          )
          transitions Regexp.empty
          in
          if TransSet.is_empty transitions then
            if i == j then
              Regexp.letter Stt.Typ.empty
            else
              regex
          else
            if i == j && not @@ TransSet.mem (i, Stt.Typ.empty, j) transitions then
              Regexp.(union regex @@ letter Stt.Typ.empty)
            else
              regex
      ) 
      line 
      in
      line
  )
  in
  let mat2 = Array.make_matrix n n Regexp.empty in
  let choose_mat = ref true in
  let () =
    for k = 0 to n-1 do
      for p = 0 to n-1 do
        for q = 0 to n-1 do
          let algo (mat : Regexp.t_simp array array)
                   (mat' : Regexp.t_simp array array) : unit =
            mat'.(p).(q) <-
              if Regexp.is_empty mat.(p).(k) || Regexp.is_empty mat.(k).(q) then
                mat.(p).(q)
              else if Regexp.is_empty mat.(p).(q) then
                if Regexp.is_empty mat.(k).(k) then
                  Regexp.letter Stt.Typ.empty
                else
                  Regexp.(
                       concat mat.(p).(k) 
                    @@ concat (star mat.(k).(k)) mat.(k).(q)
                  )
              else if k = p then
                Regexp.(concat (star mat.(p).(p)) mat.(p).(q))
              else if k = q then
                Regexp.(
                     concat mat.(p).(q)
                  @@ star mat.(q).(q)
                )
              else
                Regexp.(
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
        (acc : Regexp.t_simp) : Regexp.t_simp ->
      StateSet.fold (
        fun (end_state : state)
            (acc' : Regexp.t_simp) : Regexp.t_simp ->
          if Regexp.is_empty acc' then
            if start_state = end_state then
              Regexp.(
                  union (letter Stt.Typ.empty) (
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
              Regexp.(
                   union acc' 
                @@ union (letter Stt.Typ.empty) (
                  if !choose_mat then 
                    mat1.(start_state-1).(end_state-1) 
                  else 
                    mat2.(start_state-1).(end_state-1)
                )
              )
            else
              Regexp.union acc' (
                if !choose_mat then 
                  mat1.(start_state-1).(end_state-1) 
                else 
                  mat2.(start_state-1).(end_state-1)
              )
      )
      automaton.ends acc
  )
  automaton.starts Regexp.empty