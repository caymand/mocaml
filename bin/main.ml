[%%ml let mul_ml n m =
        if [%lift 1 n] < [%lift 1 1]
        then [%lift 2 0]
        else
          [%add 2
            [%lift 2 m]
            [%app 2
                (mul_ml
                   [%sub 1 [%lift 1 n] [%lift 1 1]]
                   [%lift 2 m])]]
]
let mul_ml' = [%run mul_ml 7]
let res = [%run mul_ml' 6]
let () =
  (* print_int @@ elm [0, 40, 41, 42, 43]; *)
  ()


