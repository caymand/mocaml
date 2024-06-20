(* [%%ml let sum_scale s b a = *)
(*         if [%lift 3 a] < [%lift 1 2 b] *)
(*         then [%add 3 *)
(*             [%mul 3 [%lift 2 1 s] [%lift 3 a]] *)
(*             [%app 3 (sum_scale *)
(*                        [%lift 1 s] *)
(*                        [%lift 2 b] *)
(*                        [%add 3 [%lift 3 1] [%lift 3 a]])] *)
(*         ] *)
(*         else [%lift 3 0] *)
(* ] *)

(* This is broken because the thing that gets "smaller" in the recursive call,
   (argument a) doe not have the largest binding time. *)
[%%ml let sum_scale_broken s a b =
        if [%lift 1 2 a] < [%lift 3 b]
        then [%add 3
            [%lift 1 2 [%mul 2 [%lift 1 1 s] [%lift 2 a]]]
            [%app 3 (sum_scale_broken
                       [%lift 1 s]
                       [%add 2 [%lift 2 1] [%lift 2 a]]
                       [%lift 3 b])]
        ]
        else [%lift 3 0]
]

[%%ml let sum_scale s a b =
        if [%lift 3 b] < [%lift 1 2 a]
        then [%lift 3 0]
        else [%add 3
            [%mul 3 [%lift 2 1 s] [%lift 3 b]]
            [%app 3 (sum_scale
                       [%lift 1 s]
                       [%lift 2 a]
                       [%sub 3 [%lift 3 1] [%lift 3 b]])]
        ]        
]

let res = [%run sum_scale_broken 0]
let res_broken = [%run res 0]

let res1 = [%run sum_scale 2]
let sum_to = [%run res1 0]
let res3 = [%run sum_to 3]

let () =
  let a = res1 0 3 in
  let b = sum_to 3 in
  Printf.printf "%d %d %d\n" a b res3;
  ()
