type program
type state

val create_state : ?num_cells:int -> unit -> state
val compile : string -> program
val run : state -> program -> unit
