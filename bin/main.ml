

let () =
  let res = [%plus 2 [%plus 2 3]] in  
  Printf.printf "res: %d\n" res
