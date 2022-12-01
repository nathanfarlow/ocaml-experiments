open Js_of_ocaml

(* A solution for https://leetcode.com/problems/valid-parentheses/ *)

let isValid s =
  let rec loop stack i =
    if i = String.length s then stack = []
    else
      match s.[i] with
      | '(' | '[' | '{' -> loop (s.[i] :: stack) (i + 1)
      | ')' | ']' | '}' -> (
          match stack with
          | [] -> false
          | c :: stack ->
              if
                (c = '(' && s.[i] = ')')
                || (c = '[' && s.[i] = ']')
                || (c = '{' && s.[i] = '}')
              then loop stack (i + 1)
              else false)
      | _ -> loop stack (i + 1)
  in
  loop [] 0

let _ =
  Js.Unsafe.global##.isValid := fun s -> Js.to_string s |> isValid |> Js.bool
