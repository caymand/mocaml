let rec sum n =
  if n < 1
  then 0
  else n + s

[%%ml let sum_ml n =
        if  [%lift 1 n] < [%lift 1 1]
        then [%lift 1 0]
        else
          [%add 1
              [%lift 1 n]
              [%app 1 (sum [%sub 1 [%lift 1 n] [%lift 1 1]])]]
]
let res = [%run sum_ml 5]
let () =
  (* print_int @@ elm [0, 40, 41, 42, 43]; *)
  ()


