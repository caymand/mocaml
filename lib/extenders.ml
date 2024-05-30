open Ppxlib
(* open Ast_builder.Default *)


let plus_ext = "plus"
let minus_ext = "-"

let () =  
  let rule = Context_free.Rule.extension @@
    Extension.V3.declare
      plus_ext
      Extension.Context.expression
      Ast_pattern.(single_expr_payload __)
      Matcher.match_plus in
  Driver.register_transformation ~rules:[rule] "expression";


    
    
    

