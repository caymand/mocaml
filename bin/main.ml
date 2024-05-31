

let sum a b = [%plus 1 a b]


let () =  
  let res = [%plus 0 2 3] in  
  Printf.printf "res: %d\n" res
