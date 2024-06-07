open! Mocaml.Extenders

(* [%%ml let add_ml a b c = [%plus 3 *)
(*           [%lift 1 2 *)
(*               [%plus 2 *)
(*                   [%lift 1 1 a] *)
(*                   [%lift 2 b]]] *)
(*           [%lift 3 c]]] *)

[%%ml let add_ml a b = [%plus 2
          [%lift 1 1 a]
          [%lift 2 b]]]

let add = [%run add_ml 4]
let add' = [%run add 2]
let () =
  let _res = add 6 in
  ()
  (* print_int res; *)

(* [%%ml let foo a = [%lift 1 a]] *)
(* let res = [%run foo 42] *)
(* let () =   *)
(*   print_int res; *)
