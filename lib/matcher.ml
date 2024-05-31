open Ppxlib

let fail_for_op symb ~loc =
  Location.raise_errorf ~loc "Expected '%s'" symb (Format.std_formatter)

let fail_with text ~loc =
  Location.raise_errorf ~loc text

let match_plus ~ctxt expr =
  let loc = Expansion_context.Extension.extension_point_loc ctxt in
  match expr with
  | [%expr [%e? t] [%e? e1] [%e? e2]] ->
    begin
      match t with
      | [%expr 1] -> [%expr [%e e1] + [%e e2]]
      (* TODO: Add case for other binding times *)
      | _ -> fail_with "Invalid binding time" ~loc
    end
  | _ -> fail_for_op "+" ~loc



(* let lift ~ctxt expr = *)
(*   let loc = Expansion_context.Extension.extension_point_loc ctxt in *)
(*   (\* let ident = Ast_pattern.pexp_ident (Ast_pattern.lident)     *\) *)
(*   match expr with *)
(*   | [%expr [%e t] [%e e1]] -> *)
(*   2 *)

(*
Strategy:
   Given a binding time of the operations, then assume that the parameters have
   the same binding time. In case not - raise an error.
   Then execute what can be executed and otherwise produce code.
*)
(* let plus ctxt =  *)
(*   let loc = Expansion_context.Extension.extension_point_loc ctxt in *)
  
(*   let rewriter = object *)
(*     inherit [(String.t * Int.t) list] Ast_traverse.map_with_context as super *)

(*     method! expression ctxt = function *)
(*       | [%expr [%e? t] [%e? e1] [%e? e2]] -> begin *)
(*           match t with *)
(*           | [%expr 0] -> fail_with "invalid bt" ~loc *)
(*           | [%expr 1] -> [%expr [%e e1] + [%e e2]] *)
(*           | _ -> fail_with "foo" ~loc *)
(*         end *)
(*       | _ -> fail_with "invalid +" ~loc *)
(*   end *)
(*   in rewriter#expression *)
