open Ppxlib

let plus_ext = "plus"
let minus_ext = "-"


let global_map (structure : Ppxlib_ast.Parsetree.structure) =
  let structure' = Multi_level_ops.map_exprs structure in
  structure'
  

let run_file () =
  let impl = Driver.Instrument.make global_map ~position:Driver.Instrument.After in
  Driver.register_transformation "global" ~impl

let () =
  run_file ()
  
  
  
  (* let rule = Context_free.Rule.extension @@ *)
  (*   Extension.V3.declare *)
  (*     plus_ext *)
  (*     Extension.Context.expression *)
  (*     Ast_pattern.(single_expr_payload __) *)
  (*     match_plus in *)
  (* Driver.register_transformation ~rules:[rule] "expression"; *)
