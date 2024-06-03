open! Mocaml.Extenders

(* [%%ml] *)
[%%ml let add_ml c b = [%plus 2
          [%plus 2 [%lift 2 c] [%lift 2 b]]
          [%lift 2 [%lift 2 b]]]]

(* let add a b = a + b(\* [%plus a b] *\) *)

let add = [%run add_ml 4]

let () =  
  let res = add 2 in
  Printf.printf "res: %d\n" res
