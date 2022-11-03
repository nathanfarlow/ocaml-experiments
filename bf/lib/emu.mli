type program
type state

val create_state : int -> state
val compile : string -> program
val run : state -> program -> unit
