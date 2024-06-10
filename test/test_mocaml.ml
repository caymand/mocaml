open! Mocaml.Extenders
open Alcotest

[%%ml let add a b = [%add 2 [%lift 1 1 a] [%lift 2 b]]]
[%%ml let sub a b = [%sub 2 [%lift 1 1 a] [%lift 2 b]]]
[%%ml let div a b = [%div 2 [%lift 1 1 a] [%lift 2 b]]]
[%%ml let mul a b = [%mul 2 [%lift 1 1 a] [%lift 2 b]]]
[%%ml let branch a b =
        if [%lift 1 a] < [%lift 1 1]
        then [%lift 2 b]
        else [%lift 1 1 a]
]

[%%ml let sum_ml n =
        if  [%lift 1 n] < [%lift 1 1]
        then [%lift 1 0]
        else
          [%add 1
              [%lift 1 n]
              [%app 1 (sum [%sub 1 [%lift 1 n] [%lift 1 1]])]]
]

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
let mul_ml_res = [%run mul_ml' 6]

let add' = [%run add 40]
let add_res = [%run add' 2]
let sub' = [%run sub 44]
let sub_res = [%run sub' 2]
let div' = [%run div 84]
let div_res = [%run div' 2]
let mul' = [%run mul 21]
let mul_res = [%run mul' 2]

let sum_res = [%run sum_ml 5]

let branch_else = [%run branch 42]
let branch_then = [%run branch 0]

let test_add () =  
  check int "add" 42 add_res
let test_sub () =  
  check int "sub" 42 sub_res    
let test_div () =  
  check int "div" 42 div_res    
let test_mul () =  
  check int "mul" 42 mul_res 

let test_simple_branch_else () = check int "else" 42 (branch_else 0)
let test_simple_branch_then () = check int "then" 42 (branch_then 42)        
(* TODO: Test for binding times of branches *)

let test_app_one_arg () = check int "sum" 15 sum_res

let test_app_multiple_args () = check int "mul" 42 mul_ml_res

let () =
  run "Tests..." [
    "Arithmetic tests",
    [test_case "add" `Quick test_add;
     test_case "sub" `Quick test_sub;
     test_case "div" `Quick test_div;
     test_case "mul" `Quick test_mul
    ];
    "Branch tests",
    [test_case "then" `Quick test_simple_branch_then;
     test_case "else" `Quick test_simple_branch_else];
    "Recursion / Function application tests",
    [test_case "one arg" `Quick test_app_one_arg;
     test_case "two args" `Quick test_app_multiple_args]  
   ]
