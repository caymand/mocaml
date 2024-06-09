open Ppxlib

let run_file () =
  let position = Driver.Instrument.Before in
  let instrument = Driver.Instrument.make Multi_level_ops.map_structure ~position in
  (* TODO: make it instrument and run before the context_free *)
  Driver.register_transformation "global" ~instrument


let create_binop_rule ext_name rewriter =
  Context_free.Rule.extension @@
      Extension.V3.declare
      ext_name
      Extension.Context.expression
      Ast_pattern.(single_expr_payload __)
      rewriter     

let () =
  run_file ();
  let rule_add = create_binop_rule "add" Multi_level_ops.gen_add in
  let rule_sub = create_binop_rule "sub" Multi_level_ops.gen_sub in
  let rule_div = create_binop_rule "div" Multi_level_ops.gen_div in
  let rule_mul = create_binop_rule "mul" Multi_level_ops.gen_mul in
  let rule_lift = Context_free.Rule.extension @@
    Extension.V3.declare
      "lift"
      Extension.Context.expression
      Ast_pattern.(single_expr_payload __)
      Multi_level_ops.gen_lift in
  Driver.register_transformation ~rules:[
    rule_lift;
    rule_add;
    rule_sub;
    rule_div;
    rule_mul
  ]
    "expression";
