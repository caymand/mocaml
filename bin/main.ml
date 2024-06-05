open! Mocaml.Extenders

(* [%%ml] *)
(* [%%ml let add_ml c b = *)
(*         [%plus 2 *)
(*             [%plus 2 *)
(*                 [%lift 2 c] *)
(*                 [%lift 2 b]] *)
(*             [%lift 2 b]]] *)

[%%ml let add_ml a b = [%plus 2
          [%lift 1 1 a]         (* Has binding time t = 1 *)
          [%lift 2 b]]]         (* Binding time t = 2 *)

let add = [%run add_ml 4]
let add2 = [%run add 2]

let () =  
  let add2' = add 2 in
  Printf.printf "add2: %d add2':%d \n" add2 add2'
