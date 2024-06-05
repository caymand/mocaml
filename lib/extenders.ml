open Ppxlib

let plus_ext = "plus"
let minus_ext = "-"

let run_file () =
  let position = Driver.Instrument.Before in
  let instrument = Driver.Instrument.make Multi_level_ops.map_structure ~position in
  (* TODO: make it instrument and run before the context_free *)
  Driver.register_transformation "global" ~instrument



let () =
  run_file ();
  let rule_plus = Context_free.Rule.extension @@
    Extension.V3.declare
      plus_ext
      Extension.Context.expression
      Ast_pattern.(single_expr_payload __)
      Multi_level_ops.gen_plus in
  let rule_lift = Context_free.Rule.extension @@
    Extension.V3.declare
      "lift"
      Extension.Context.expression
      Ast_pattern.(single_expr_payload __)
      Multi_level_ops.gen_lift in
  Driver.register_transformation ~rules:[rule_lift; rule_plus] "expression";
