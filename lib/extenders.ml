open Ppxlib
open Multi_level_ops

let map_structure (structure : structure) =
  (* TODO: maybe inpalce updates. We already track these effects *)
  let module E = Algaeff.State.Make (struct type t = ml_defs list end) in
  let (let*) = Option.bind in
  let mapper = object
    inherit Ast_traverse.map as super

    method! structure_item stri =
      let loc = stri.pstr_loc in
      match stri with
      | [%stri [%%ml let [%p? decl]  = [%e? expr]]] -> begin
          match decl.ppat_desc with
          | Ppat_var loc' ->
            (* TODO: Here it could also make sense to traverse the function
               and build ml_ops. *)
            let fname = loc'.txt in
            let def = { name = fname; expr = expr } in
            E.modify (fun defs -> def::defs);                        
            [%stri []]
          | _ -> Codegen.fail_with "invalid multi level declaration" ~loc
        end
      | [%stri let [%p? lhs] = [%run [%e? f] [%e? expr]]] -> begin
          match f.pexp_desc with
          | Pexp_ident ident ->
            let fname = match ident.txt with | Lident fname -> fname | _ -> "" in
            let specialization =
              let* ml_def = List.find_opt (fun def -> String.equal fname def.name) (E.get ()) in
              let specialization = Pe.specialize ml_def.expr expr in
              print_endline @@ "specialization" ^ Pprinter.show_exp specialization;
              (* The new specialization can also be specialized (if it takes morea arguments) *)
              let new_fun_decl = {name = (var_name lhs.ppat_desc); expr = specialization} in
              E.modify (fun defs -> new_fun_decl::defs);
              Option.some specialization
            in
            begin
              match specialization with
              | Some expr -> [%stri let [%p lhs] = [%e expr]]
              | None -> (Codegen.fail_with ("Multi level decl for " ^ fname ^ " not found") ~loc)
            end
          | _ -> Codegen.fail_with "run not impl for more than 1 argument to specializer" ~loc
        end
      | _ -> super#structure_item stri
  end
  in
  let res = E.run ~init:[] (fun () -> mapper#structure structure) in
  print_endline "-----";
  print_endline @@ Pprinter.show_strct res;
  print_endline "-----";
  res

let run_file () =
  let position = Driver.Instrument.Before in
  let instrument = Driver.Instrument.make map_structure ~position in
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
  let rule_add = create_binop_rule "add" Codegen.gen_add in
  let rule_sub = create_binop_rule "sub" Codegen.gen_sub in
  let rule_div = create_binop_rule "div" Codegen.gen_div in
  let rule_mul = create_binop_rule "mul" Codegen.gen_mul in
  let rule_lift = Context_free.Rule.extension @@
    Extension.V3.declare
      "lift"
      Extension.Context.expression
      Ast_pattern.(single_expr_payload __)
      Codegen.gen_lift in
  Driver.register_transformation ~rules:[
    rule_lift;
    rule_add;
    rule_sub;
    rule_div;
    rule_mul
  ]
    "expression";
