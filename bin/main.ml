open! Mocaml.Extenders

(* [%%ml] *)
(* [%%ml let add_ml c b = *)
(*         [%plus 2 *)
(*             [%plus 2 *)
(*                 [%lift 2 c] *)
(*                 [%lift 2 b]] *)
(*             [%lift 2 b]]] *)

[%%ml let add_ml a b = [%plus 2
          [%lift 2 a]
          [%lift 2 b]]]

let add = [%run add_ml 4]

let () =  
  let res = add 2 in
  Printf.printf "res: %d\n" res
