open Ppxlib

let fail_with text ~loc =
  Location.raise_errorf ~loc "%s" text

let pp_pattern fmt pat =
  Pprintast.pattern fmt pat

let pp_expression fmt expr =
  Pprintast.expression fmt expr

let pp_structure_item fmt stri =
  Pprintast.structure_item fmt stri

let pp_structure fmt str =
  Pprintast.structure fmt str

let show_exp = [%show: expression]
let show_pat = [%show: pattern]
let show_strct = [%show: structure]
let show_strcti = [%show: structure_item]

type ml_defs = {
  name : string;
  expr : expression
}[@@deriving show]

(* Primitive types that each expression can take. *)
type ml_val = Val of int | Ident of string [@@deriving show, eq]
(* Wrapper to store binding time info *)
and ml_expr = {
  v: ml_val;
  t: int;
  s: int;
} [@@deriving show]

(* Supported Ops *)
type ml_ops = Add of int * ml_ops * ml_ops
            | Expr of ml_expr
            | Lift of ml_ops
            | Fun of string * ml_ops
[@@deriving show, eq]

module Errors = struct
  let invalid_binding_times ~e1 ~e2 ~e1' ~e2' =
    Printf.sprintf {|ICE. Binding times did not match. Before:
%s
%s
After:
%s
%s
 |}(show_ml_ops e1) (show_ml_ops e2) (show_ml_ops e1') (show_ml_ops e2')
end


module E = Algaeff.State.Make (struct type t = (string * int) list end)

let eval_leaf = function
  | Val v -> Some v
  | Ident ident ->
    List.assoc_opt ident @@ E.get ()

let var_name = function
  | Ppat_var ident -> ident.txt
  | _ -> failwith "Only normal functions fun x -> body can be specialized"

let rec bt_of_ops = function
  | Add (t, _, _) -> t
  | Expr {v=_; t; s} -> t + s
  | Lift op -> 1 + bt_of_ops op
  | Fun (_, body) -> bt_of_ops body

(* This is not really replacing, so much as it is in fact
   the actual specialization. *)
let replace ~ident ~with_val in_op = let (let*) = Result.bind in
  let decrease_bt e v =
    if e.t > 0
    then {v; t = (e.t - 1); s=e.s}
    else {v; t=0; s=(e.s - 1)} in

  let rec go op = 
    match op with
    | Add (t, e1, e2) ->
      let* e1' = go e1 in
      let* e2' = go e2 in
      if (bt_of_ops e1') = (bt_of_ops e2') 
      then Result.ok @@ Add (t-1, e1', e2')
      else Result.error @@ Errors.invalid_binding_times ~e1 ~e2 ~e1' ~e2'        
    | Expr e when equal_ml_val e.v ident ->
      Result.ok @@ Expr (decrease_bt e with_val)
    | Expr e -> Result.ok @@ Expr (decrease_bt e e.v)
    | Fun (a, body) ->
      let* body' = go body in
      Result.ok @@ Fun (a, body')
    | _ -> Result.ok op    
  in
  go in_op
(* For all generation functions, they are fully annotated and they have been
   specialized. Therefore, we could also do binding time analysis on these.*)
let gen_plus ~ctxt expr =
  let loc = Expansion_context.Extension.extension_point_loc ctxt in
  match expr with
  | [%expr [%e? t] [%e? e1] [%e? e2]] ->
    (* TODO: Consider binding times *)
    [%expr [%e e1] + [%e e2]]
  | e ->
    let msg = Printf.sprintf
        "failed generating code for plus: %s"
        (show_exp e) in
    fail_with msg ~loc

let gen_lift ~ctxt expr =
  let loc = Expansion_context.Extension.extension_point_loc ctxt in
  match expr with
  | [%expr [%e? _t] [%e? e]] -> [%expr [%e e]]
  | [%expr [%e? _t] [%e? _s] [%e? e]] -> [%expr [%e e]]
  | _ -> fail_with ~loc "Invalid lift"

let gen_val ~loc = function
  | Val v ->        
    Ast_builder.Default.eint v ~loc
  | Ident id -> (Ast_builder.Default.evar id ~loc)

let rec cogen ~loc = function
  | Add (t, e1, e2) ->
    let t' = (Ast_builder.Default.eint t ~loc)
    and e1' = cogen e1 ~loc        
    and e2' = cogen e2 ~loc in
    [%expr [%plus [%e t'] [%e e1'] [%e e2']]]
  | Expr e -> 
      let s = (Ast_builder.Default.eint e.s ~loc) 
      and t = (Ast_builder.Default.eint e.t ~loc)
      and expr = gen_val ~loc e.v in
      [%expr [%lift [%e t] [%e s] [%e expr]]]
  | Lift e -> cogen e ~loc
  | Fun (a, body) ->
    let a' = Ast_builder.Default.ppat_var ~loc (Loc.make ~loc a) in
    let body' = cogen body ~loc in
    [%expr fun [%p a'] -> [%e body']]

(* Generate code for the specialization of a function *)
let bt_of_pexp_desc = function
  | Pexp_constant (Pconst_integer (s, _)) -> int_of_string s
  | _ ->
    print_endline "Expected constant of the bt";
    failwith "
bt must be a constant"

let create_ml_expr ?(t = 0) ?(s=0) (pexp_desc : expression_desc) =
  match pexp_desc with
  | Pexp_constant (Pconst_integer (i, _)) ->
    let v = Val (int_of_string i) in
    {v; t; s}
  | Pexp_ident {txt =(Lident var); loc=_loc} ->
    {v=(Ident var); t; s}
  | _ ->    
    failwith "ICE. Cannot create ml_expr from the pexp_desc"
      
module S = Algaeff.State.Make (struct type t = ml_ops end)

let lift_plus t e1 e2 ~traverse = 
  let _ = traverse e1 in let e1' = S.get () in 
  let _ = traverse e2 in let e2' = S.get() in
  S.set @@ Add (bt_of_pexp_desc t.pexp_desc, e1',  e2')

let specialize (to_specialize : expression) (arg : expression) : expression =  
  let lift v ident =
    object           
      inherit Ast_traverse.iter as super

      (* Traverse the extension notes*)
      method! extension ext =
        let (ext_loc, payload) = ext in
        let _loc = ext_loc.loc in
        match payload with
        | PPat (_pat, Some _expr) ->
          super#extension ext
        | PStr strct -> begin          
            match ext_loc.txt with
            | "plus" -> begin
                match strct with
                | [%str [%e? t] [%e? e1] [%e? e2]] ->
                  lift_plus t e1 e2 ~traverse:(super#expression)
                | _ ->                
                  failwith "Incorrect format"
              end
            | "lift" -> begin
                match strct with
                | [%str [%e? t] [%e? e]] ->
                  let bt = bt_of_pexp_desc t.pexp_desc in                
                  let ml_expr = create_ml_expr e.pexp_desc ~t:bt in
                  S.set (Expr ml_expr)
                | [%str [%e? t] [%e? s] [%e? e]] ->
                  let bt = bt_of_pexp_desc t.pexp_desc in
                  let s' = bt_of_pexp_desc s.pexp_desc in
                  let ml_expr = create_ml_expr e.pexp_desc ~t:bt ~s:s' in
                  S.set (Expr ml_expr)
                | _ -> failwith "incorrect format"
              end
            | _ ->
              failwith "extension not yet implemented"
          end
        | _ ->
          super#extension ext
      (* Traverse expressions that we try to specialize.
         The expression will either be a function, where we try to specialize
         one of the arguments, or a multilevel expression. The later happens if there
         is only one argument left to sepcialize.*)
      method! expression expr =
        print_endline @@ "handling expr: " ^ (show_exp expr);
        match expr with
        | [%expr fun [%p? p] -> [%e? rest]] ->                
          super#expression rest;
          let body = S.get () in
          print_endline @@ "body: " ^ show_ml_ops body;
          S.set @@ Fun (var_name p.ppat_desc, body);        
          (* TODO: more cases should be handled *)
        | [%expr [%plus [%e? _t] [%e? _e1] [%e? _e2]]] ->
          lift_plus _t _e1 _e2 ~traverse:(super#expression)
        | _ ->
          begin          
            match expr.pexp_desc with
            | Pexp_constant (Pconst_integer (s, _)) ->
              let v = int_of_string s in
              S.set @@ Expr {v=(Val v); t = 0; s = 0}          
            | _ ->
              print_endline @@ "unexpected: " ^ show_exp expr;
              failwith "not implemented here"
          end
    end
  in
  let arg_ml_expr = create_ml_expr arg.pexp_desc in
  let loc = to_specialize.pexp_loc in
  (* Traverse the tree inside an effect handler to collect states *)
  S.run ~init:(Expr arg_ml_expr) (fun () ->
      match to_specialize with
      (* For the function pattern, ident is the argument that is specialized. *)
      | [%expr fun [%p? ident] -> [%e? rest]] -> begin
          print_endline @@ "args: " ^ show_pat ident;
          print_endline @@ "rest: " ^ show_exp rest;
          match ident.ppat_desc with
          | Ppat_var ident_loc ->
            let lift_body = lift arg_ml_expr.v (Ident ident_loc.txt) in
            lift_body#expression rest;
            let specialization = replace
                ~ident:(Ident ident_loc.txt)
                ~with_val:arg_ml_expr.v
                (S.get ()) in            
            (* After specializing a function, we have to make sure that the
               arguments to the function are still in scope.*)
            print_endline @@ "Spec: "^show_ml_ops (Result.get_ok specialization);
            Result.fold
              ~ok:(cogen ~loc)
              ~error:(fail_with ~loc:rest.pexp_loc)
              specialization
          | _ -> fail_with "invalid type" ~loc:ident.ppat_loc
        end
      | _ -> fail_with "wrong type" ~loc:to_specialize.pexp_loc
    )

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
            (* [%stri let [%p decl]  = failwith "You cannot use this function. Specialize it first"] *)
            [%stri []]
          | _ -> fail_with "invalid multi level declaration" ~loc
        end
      | [%stri let [%p? lhs] = [%run [%e? f] [%e? expr]]] -> begin
          match f.pexp_desc with
          | Pexp_ident ident ->
            let fname = match ident.txt with | Lident fname -> fname | _ -> "" in
            let specialization =
              let* ml_def = List.find_opt (fun def -> String.equal fname def.name) (E.get ()) in
              let specialization = specialize ml_def.expr expr in
              print_endline @@ "specialization" ^ show_exp specialization;
              (* The new specialization can also be specialized (if it takes morea arguments) *)
              let new_fun_decl = {name = (var_name lhs.ppat_desc); expr = specialization} in
              E.modify (fun defs -> new_fun_decl::defs);
              Option.some specialization
            in
            begin
              match specialization with
              | Some expr -> [%stri let [%p lhs] = [%e expr]]
              | None -> (fail_with ("Multi level decl for " ^ fname ^ " not found") ~loc)
            end
          | _ -> fail_with "run not impl for more than 1 argument to specializer" ~loc
        end
      | _ -> super#structure_item stri
  end
  in
  let res = E.run ~init:[] (fun () -> mapper#structure structure) in
  print_endline "-----";
  print_endline @@ show_strct res;
  print_endline "-----";
  res
