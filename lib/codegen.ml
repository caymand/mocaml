open! Ppxlib

let fail_with text ~loc =
  Location.raise_errorf ~loc "%s" text

let gen_binop ~ctxt ~oper expr =
  let loc = Expansion_context.Extension.extension_point_loc ctxt in
  match expr with
  | [%expr [%e? t] [%e? e1] [%e? e2]] ->    
    (* TODO: Consider binding times *)
    [%expr [%e oper] [%e e1] [%e e2]]
  | e ->
    let msg = Printf.sprintf
        "failed generating code for: %s"
        (Pprinter.show_exp e) in
    fail_with msg ~loc

let gen_add ~ctxt =
  let loc = Expansion_context.Extension.extension_point_loc ctxt in
  gen_binop ~ctxt ~oper:[%expr Int.add]
let gen_sub ~ctxt =
  let loc = Expansion_context.Extension.extension_point_loc ctxt in
  gen_binop ~ctxt ~oper:[%expr Int.sub]
let gen_div ~ctxt =
  let loc = Expansion_context.Extension.extension_point_loc ctxt in
  gen_binop ~ctxt ~oper:[%expr Int.div]
let gen_mul ~ctxt =
  let loc = Expansion_context.Extension.extension_point_loc ctxt in
  gen_binop ~ctxt ~oper:[%expr Int.mul]

let gen_lift ~ctxt expr =
  let loc = Expansion_context.Extension.extension_point_loc ctxt in
  match expr with
  | [%expr [%e? _t] [%e? e]] -> [%expr [%e e]]
  | [%expr [%e? _t] [%e? _s] [%e? e]] -> [%expr [%e e]]
  | _ -> fail_with ~loc "Invalid lift"

let gen_val ~loc leaf = let open Multi_level_ops in match leaf with
  | Val v ->        
    Ast_builder.Default.eint v ~loc
  | Ident id -> (Ast_builder.Default.evar id ~loc)

let rec cogen ~loc op = let open Multi_level_ops in match op with
  (* | Add (t, e1, e2) -> *)
  | Binop (t, binop) ->
    let (e1, e2, _) = construct_binop binop in
    let t' = (Ast_builder.Default.eint t ~loc) in
    let e1' = cogen e1 ~loc in
    let e2' = cogen e2 ~loc in
    begin match binop with
      | Add _ ->
        [%expr [%add [%e t'] [%e e1'] [%e e2']]]
      | Sub _ ->
        [%expr [%sub [%e t'] [%e e1'] [%e e2']]]
      | Div _ ->
        [%expr [%div [%e t'] [%e e1'] [%e e2']]]
      | Mul _ ->
        [%expr [%mul [%e t'] [%e e1'] [%e e2']]]
    end
  | Expr e ->
    let t = Ast_builder.Default.eint e.t ~loc in
    let expr = gen_val ~loc e.v in
    [%expr [%lift [%e t] [%e expr]]]
  | Lift (s, e) ->    
    let e = match e with
      | Expr e -> [%expr [%e (gen_val e.v ~loc)]]
      | _ -> cogen e ~loc
    and t = Ast_builder.Default.eint ~loc @@ bt_of_ops e 
    and s' = Ast_builder.Default.eint s ~loc in
    [%expr [%lift [%e s'] [%e t] [%e e]]]
  | Fun (a, body) ->
    let a' = Ast_builder.Default.ppat_var ~loc (Loc.make ~loc a) in
    let body' = cogen body ~loc in
    [%expr fun [%p a'] -> [%e body']]
  | IfElse (cond, e_then, e_else) ->
    let cond' = match cond with
      | Leq (e1, e2) ->
        let e1' = cogen e1 ~loc in
        let e2' = cogen e2 ~loc in
        [%expr [%e e1'] < [%e e2']]
      | _ -> fail_with "unexpected conditional" ~loc
    in
    let e_then' = cogen e_then ~loc in
    let e_else' = cogen e_else ~loc in
    [%expr if [%e cond'] then [%e e_then'] else [%e e_else']]
  | App (_t, fn, args) ->
    let arg_labels = List.map (fun arg -> (Nolabel, cogen arg ~loc)) args in
    Ast_builder.Default.pexp_apply fn arg_labels ~loc
