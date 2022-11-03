open Bf.Emu

let () =
  if Array.length Sys.argv < 2 then failwith "Usage: bf <file>"
  else
    let program = Sys.argv.(1) |> Core.In_channel.read_all |> compile in
    let state = create_state 30000 in
    run state program
