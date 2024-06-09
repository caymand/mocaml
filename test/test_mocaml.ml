open! Mocaml.Extenders

[%%ml let add a b = [%add 2 [%lift 1 1 a] [%lift 2 b]]]
[%%ml let sub a b = [%sub 2 [%lift 1 1 a] [%lift 2 b]]]
[%%ml let div a b = [%div 2 [%lift 1 1 a] [%lift 2 b]]]
[%%ml let mul a b = [%mul 2 [%lift 1 1 a] [%lift 2 b]]]

let add' = [%run add 40]
let add_res = [%run add' 2]
let sub' = [%run sub 44]
let sub_res = [%run sub' 2]
let div' = [%run div 84]
let div_res = [%run div' 2]
let mul' = [%run mul 21]
let mul_res = [%run mul' 2]

let match_tests ~actual ~expected =
  Array.iter2 (fun expected actual ->
      match expected = actual with
      | true -> ()
      | false -> Printf.printf "Expected: %d, got %d" expected actual
    )
    expected
    actual

let () =
  let actuals = Array.of_list [add_res; sub_res; div_res; mul_res] in
  match_tests
    ~actual:actuals
    ~expected:(Array.make (Array.length actuals) 42)
