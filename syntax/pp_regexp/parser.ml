module S = struct

  type symbol = string
  type t = string

  let compare : t -> t -> int = String.compare
  let pp (s : t) : string = s

  let epsilon = "EPS"
  let is_epsilon (s : t) : bool = 
    s = epsilon

  let get (s : t) : symbol option =
    if is_epsilon s then
      None
    else
      Some s
  let symbol (s : symbol) : t = s

end

module A = Automaton.Make(S)

let parse_file (file_name : string) : string * A.t =
  let file = open_in file_name in
  try
    let regex = input_line file in
    let nb_states = int_of_string @@ input_line file in
    let ends = List.map int_of_string @@ String.split_on_char ',' @@ input_line file in
    let transitions = ref [] in
    let () =
      try
        while true do
          let f (line : string) : int * string * int =
            match String.split_on_char ',' line with
            | s1 :: l :: s2 :: [] ->
              (int_of_string s1, l, int_of_string s2)
            | _ -> failwith "Format error : line must be \"x,x,x\""
          in
          transitions := (f @@ input_line file) :: !transitions
        done
      with End_of_file ->
        close_in file
    in
    let letters_with_epsilon = List.map (
      fun (_, l, _ : int * string * int) : S.t ->
        S.symbol l
    )
    !transitions
    in
    let letters = List.filter (
      fun (l: S.t) : bool ->
        not @@ S.is_epsilon l
    ) 
    letters_with_epsilon
    in
    let automaton = A.create letters in
    let states = ref [] in
    let () =
      for i = 0 to nb_states-1 do
        states := i :: !states
      done
    in
    let automaton = A.add_states automaton !states in
    let automaton = A.add_start automaton 0 in
    let automaton = A.add_ends automaton ends in
    let automaton = A.add_transitions automaton !transitions in
    regex, automaton
  with e ->
    close_in_noerr file ;
    raise e