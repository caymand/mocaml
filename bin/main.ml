[%%ml let mul_ml_dynamic_if n m =
        if [%lift 2 m] < [%lift 2 1]
        then [%lift 2 0]
        else
          [%add 2
              [%lift 2 n]
              [%app 2
                  (mul_ml_dynamic_if                     
                     [%lift 1 n]
                     [%sub 2 [%lift 2 m] [%lift 2 1]])]]
]

let res = [%run mul_ml_dynamic_if 7]
let __res = [%run res 7]

let () =
  Printf.printf "Res. %d\n" (res 7);
