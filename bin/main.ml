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


(* assuming n = 3
   [%lift 1 (List.nth alist [%lift 1 n])]]
   -> List.nth alist [%lift 1 n]
   -> List.nth alist 3


   We can sort of treat these general OCaml expressions as templates.
   We then need to "fill the hole" with the partial_evaluation of the ops.

   Again say we want to eval this:
     [%lift 1 (List.nth alist [%lift 1 n])]]

   Then the interesting part is the template (List.nth alist [%lift 1 n])
   When we traverse this we will go in the order:
   - List.nth alist [%lift 1 n]
   - List.nth
   - alist
   - [%lift 1 n]

   What we want to do take (List.nth alist [%lift 1 n]) and replace whatever we can.
   So what we can do is generate this
   (Templ (List.nth alist [%lift 1 n]),
     (Tmpl List.nth,
       (Tmpl alist,
         n)))
   ->
   (Templ (List.nth alist [%lift 1 n]),
     (Tmpl List.nth,
       (Tmpl alist,
         Expr 3)))


   What ends up happening is this
   [%lift 1 (List.nth alist [%lift 1 n])]]
   ->
   (List.nth alist [%lift 1 n]) -> Tem
   ->
   (List.nth) 
   ->
   alist : Expr 3
   ->
   [%lift 1 n] : Expr n
   
 *)
