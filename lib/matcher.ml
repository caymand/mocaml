open Ppxlib

let fail_for_op symb ~loc =
  Location.raise_errorf ~loc "Expected '%s'" symb (Format.std_formatter)

let match_plus ~ctxt expr =
  let loc = Expansion_context.Extension.extension_point_loc ctxt in
  match expr with
  | [%expr [%e? _t] [%e? _e1] [%e? e2]] ->
    [%expr 1 + [%e e2]]
  | _ -> fail_for_op "+" ~loc



  (* let rec process_expr expr = *)
  (*   match expr with *)
  (*   | [%expr [%e? _] [%e? e2]] -> *)
  (*     let e2' = process_expr e2 in *)
  (*     [%expr 1 + [%e e2']] *)
  (*   | _ -> expr *)
  (* in *)
           
(* let traverse ~ctxt expr = *)
(*   let loc = Expansion_context.Extension.extension_point_loc ctxt in *)
(*   let foo = object *)
(*     inherit [Int.t] Ast_traverse.fold as super *)

(*     method! expression e acc = *)
(*       let acc = super#expression e acc in *)
(*       match e with *)
(*       | [%expr [%e? _] [%e? _]] -> acc + 1 *)
(*       | _ -> super#expression e acc *)

(*   end *)
(*   in foo# *)
