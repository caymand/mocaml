open Ppxlib

let fail_for_op symb ~loc =
  Location.raise_errorf ~loc "Expected '%s'" symb (Format.std_formatter)

let fail_with text ~loc =
  Location.raise_errorf ~loc text

let match_binop ~ctxt ~oper expr =
  let loc = Expansion_context.Extension.extension_point_loc ctxt in
  match expr with
  | [%expr [%e? t] [%e? e1] [%e? e2]] ->
    begin
      match t with
      | [%expr 1] -> [%expr [%p oper] [%e e1] [%e e2]]
      (* TODO: Add case for other binding times *)
      | _ -> fail_with "Invalid binding time" ~loc
    end
  | _ -> fail_for_op "+" ~loc



let match_plus ~ctxt expr =
  match_binop ~ctxt ~oper:(Int.add) expr
let match_sub ~ctxt expr =
  match_binop ~ctxt ~oper:(Int.sub) expr
let match_div ~ctxt expr =
  match_binop ~ctxt ~oper:(Int.div) expr
let match_mul ~ctxt expr =
  match_binop ~ctxt ~oper:(Int.mul) expr
