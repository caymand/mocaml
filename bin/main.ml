open Mocaml


let () =
  let res = [%plus 0 2 3] in
  Printf.printf "res: %d\n" res
