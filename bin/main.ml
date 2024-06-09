open! Mocaml.Extenders

(* [%%ml let add_ml a b c = if [%lift 2 b] < [%lift 1 1 a] *)
(*         then ([%plus 3 *)
(*           [%lift 1 2 *)
(*               [%plus 2 *)
(*                   [%lift 1 1 a] *)
(*                   [%lift 2 b]]] *)
(*           [%lift 3 c]]) *)
(*         else 2 ] *)

(* [%%ml let iprod n a b = *)
(*         if [%lift 1 0 0] < [%lift 1 n] *)
(*         then *)
(*           [%plus 2 *)
(*               [%mul 2 *)
(*                   [%lift 1 1 []]] *)
(*           ] *)
(*         else [%lift 2 0] *)
(* ] *)

[%%ml let idx n alist =        
        [%app 2 (List.nth [%lift 2 alist] [%lift 1 1 n])]
]

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

let elm = [%run idx 3]

let () =
  (* print_int @@ elm [0, 40, 41, 42, 43]; *)
  ()
