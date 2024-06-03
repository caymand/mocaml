open Ppxlib

let plus_ext = "plus"
let minus_ext = "-"

let run_file () =  
  let impl = Multi_level_ops.map_structure in
  (* TODO: make it instrument and run before the context_free *)
  Driver.register_transformation "global" ~impl

let () = run_file ()
  
  
  
  (* let rule = Context_free.Rule.extension @@ *)
  (*   Extension.V3.declare *)
  (*     plus_ext *)
  (*     Extension.Context.expression *)
  (*     Ast_pattern.(single_expr_payload __) *)
  (*     match_plus in *)
  (* Driver.register_transformation ~rules:[rule] "expression"; *)
