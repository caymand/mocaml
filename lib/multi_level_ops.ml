open Ppxlib

let fail_for_op symb ~loc =
  Location.raise_errorf ~loc "Expected '%s'" symb (Format.std_formatter)

let fail_with text ~loc =
  Location.raise_errorf ~loc text
    
type expr_info =
  {
    expr: expression;
    loc: Location.t;
  }

let rec map_exprs (structure: Ppxlib_ast.Parsetree.structure) ~acc = match structure with
  | [] -> acc
  | stri :: _rest -> begin
      match stri.pstr_desc with
      | Pstr_extension (({txt = "run"; _}, PStr [{pstr_desc = (Pstr_module _mb); _}]), _) ->
        fail_with "foo"
      | _ -> fail_with "not implemented"
    end
