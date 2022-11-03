type op =
  | MOVE_RIGHT
  | MOVE_LEFT
  | INC
  | DEC
  | OUTPUT
  | INPUT
  | LOOP of op list

type program = op list
type state = { data : int array; mutable ptr : int }

exception Out_of_bounds

let split_loop program =
  let rec aux program acc depth =
    match program with
    | [] -> failwith "Unmatched ["
    | '[' :: tl -> aux tl ('[' :: acc) (depth + 1)
    | ']' :: tl when depth = 0 -> (List.rev acc, tl)
    | ']' :: tl -> aux tl (']' :: acc) (depth - 1)
    | hd :: tl -> aux tl (hd :: acc) depth
  in
  aux program [] 0

let rec compile program =
  match program with
  | [] -> []
  | '>' :: rest -> MOVE_RIGHT :: compile rest
  | '<' :: rest -> MOVE_LEFT :: compile rest
  | '+' :: rest -> INC :: compile rest
  | '-' :: rest -> DEC :: compile rest
  | '.' :: rest -> OUTPUT :: compile rest
  | ',' :: rest -> INPUT :: compile rest
  | '[' :: rest ->
      let loop, rest = split_loop rest in
      LOOP (compile loop) :: compile rest
  | ']' :: _ -> failwith "Unmatched ]"
  | _ :: rest -> compile rest

let compile program =
  compile (List.init (String.length program) (String.get program))

let rec run state program =
  match program with
  | [] -> ()
  | MOVE_RIGHT :: rest when state.ptr + 1 < Array.length state.data ->
      state.ptr <- state.ptr + 1;
      run state rest
  | MOVE_RIGHT :: _ -> raise Out_of_bounds
  | MOVE_LEFT :: rest when state.ptr > 0 ->
      state.ptr <- state.ptr - 1;
      run state rest
  | MOVE_LEFT :: _ -> raise Out_of_bounds
  | INC :: rest ->
      state.data.(state.ptr) <- state.data.(state.ptr) + 1;
      run state rest
  | DEC :: rest ->
      state.data.(state.ptr) <- state.data.(state.ptr) - 1;
      run state rest
  | OUTPUT :: rest ->
      print_char (Char.chr state.data.(state.ptr));
      run state rest
  | INPUT :: _ -> failwith "Input not implemented"
  | LOOP loop :: rest ->
      if state.data.(state.ptr) = 0 then run state rest
      else (
        run state loop;
        run state program)

let create_state num_cells = { data = Array.make num_cells 0; ptr = 0 }
