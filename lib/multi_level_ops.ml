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
} [@@deriving show]

(* Supported Ops *)
type ml_cond = Leq of ml_ops * ml_ops
             | Bool of bool
[@@deriving show, eq]
and ml_ops = Add of int * ml_ops * ml_ops
            | Expr of ml_expr
            | Lift of int * ml_ops
            | Fun of string * ml_ops
            | IfElse of ml_cond * ml_ops * ml_ops
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
    print_endline @@ "eval leaf ident: " ^ ident;
    List.assoc_opt ident @@ E.get ()

let var_name = function
  | Ppat_var ident -> ident.txt
  | _ -> failwith "Only normal functions fun x -> body can be specialized"

let rec bt_of_ops = function
  | Add (t, _, _) -> t
  | Expr {v=_; t} -> t
  | Lift (s, op) -> s + bt_of_ops op
  | Fun (_, body) -> bt_of_ops body
  | _ -> failwith "Cannot find bt for this expression"

(* This is not really replacing, so much as it is in fact
   the actual specialization. *)
let replace ~ident ~with_val in_op = let (let*) = Result.bind in
  (* TODO:FIXME *)
  let decrease_bt e v =
    if e.t > 0
    then {v; t = (e.t - 1)}
    else {v; t=0;} in
  
  (* TODO maybe rename to pe or partial_eval*)
  let rec eval = function
    | Add (1, e1, e2) ->
      let* v1 = eval e1 in
      let* v2 = eval e2 in
      Result.ok @@ v1 + v2
    | Lift (s, e) when s <= 1 -> eval e
    | Expr e when e.t <= 1 -> begin
        match e.v with
        | Val v -> Result.ok v
        | Ident _ -> Result.error "Cannot evaluate identifiers"
      end
    | e -> Result.error @@ "Cannot eval expr of bt > 1: " ^ show_ml_ops e;
  in

  let rec go_cond cond =    
    match cond with
    | Leq (e1, e2) ->
      print_endline @@ "cond: " ^ show_ml_cond cond;
      let* e1' = go e1 in
      let* e2' = go e2 in      
      if bt_of_ops e1' = bt_of_ops e2' && bt_of_ops e1' = 0
      then let* v1 = eval e1' in
        let* v2 = eval e2' in
        Result.ok @@ Bool (v1 < v2)
      else Result.ok @@ Leq (e1', e2')
    | b -> Result.ok b
  and go op =    
    match op with
    | Add (1, e1, e2)->
      let* e1' = go e1 in
      let* e2' = go e2 in    
      let* v = eval @@ Add (1, e1' , e2') in
      Result.ok @@ Expr {v=(Val v); t=0}
    | Add (t, e1, e2) ->
      let* e1' = go e1 in
      let* e2' = go e2 in
      if (bt_of_ops e1') = (bt_of_ops e2') 
      then Result.ok @@ Add (t-1, e1', e2')
      else Result.error @@ Errors.invalid_binding_times ~e1 ~e2 ~e1' ~e2'        
    | Expr e when equal_ml_val e.v ident ->
      if e.t = 1
      then Result.ok @@ Expr (decrease_bt e with_val)
      else Result.error @@ "ICE. Invalid BT for variable"
    | Expr e ->
      Result.ok @@ Expr (decrease_bt e e.v)
    | Fun (a, body) ->
      let* body' = go body in
      Result.ok @@ Fun (a, body')
    (* Release the inner value. s specializations has occured. *)
    | Lift (1, e) when bt_of_ops e = 0 ->
      let* e' = go e in
      Result.ok e'
    | Lift (s, e) ->
      (* In this case, if t=0 and s=0 the expression is just evaluated as is *)
      let t = bt_of_ops e in
      let* e' = go e in
      if t = 0
      then Result.ok @@ Lift (s-1, e')
      else Result.ok @@ Lift (s, e')
    | IfElse (cond, e_then, e_else) ->
      (* print_endline "if then else"; *)
      (* print_endline @@ show_ml_ops op; *)
      let* cond' = go_cond cond in
      begin
        match cond' with
        | Bool b ->
          print_endline "cogen bool";
          if b then go e_then else go e_else
        | _ ->
          print_endline "if else cogen";
          let* e_then' = go e_then in
          let* e_else' = go e_else in
          Result.ok @@ IfElse (cond', e_then', e_else')
      end     
    (* | e -> Result.error @@ "ICE. Unexpected binding times: " ^ show_ml_ops e *)
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

let rec cogen ~loc op = match op with
  | Add (t, e1, e2) ->
    let t' = (Ast_builder.Default.eint t ~loc)
    and e1' = cogen e1 ~loc        
    and e2' = cogen e2 ~loc in
    [%expr [%plus [%e t'] [%e e1'] [%e e2']]]
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

(* Generate code for the specialization of a function *)
let bt_of_pexp_desc = function
  | Pexp_constant (Pconst_integer (s, _)) -> int_of_string s
  | _ ->    
    failwith "bt must be a constant"

let create_ml_expr ?(t = 0) (expr : expression) =
  match expr.pexp_desc with
  | Pexp_constant (Pconst_integer (i, _)) ->
    let v = Val (int_of_string i) in
    Some {v; t}
  | Pexp_ident {txt =(Lident var); loc=_loc} ->
    Some {v=(Ident var); t}
  | _ -> None
    (* print_endline @@ show_exp expr; *)
    (* failwith "ICE. Cannot create ml_expr from the pexp_desc" *)
      
module S = Algaeff.State.Make (struct type t = ml_ops end)

let lift_plus t e1 e2 ~traverse =  
  let _ = traverse e1 in let e1' = S.get () in 
  let _ = traverse e2 in let e2' = S. get() in
  let e = Add (bt_of_pexp_desc t.pexp_desc, e1',  e2') in  
  S.set e

let specialize (to_specialize : expression) (arg : expression) : expression =  
  let lift v ident =
    object (self)        
      inherit Ast_traverse.iter as _super      
      (* Traverse expressions that we try to specialize.
         The expression will either be a function, where we try to specialize
         one of the arguments, or a multilevel expression. The later happens if there
         is only one argument left to sepcialize.*)
      method! expression expr =
        (* print_endline @@ "expr: " ^ (show_exp expr); *)
        match expr with
        | [%expr fun [%p? p] -> [%e? rest]] ->
          print_endline @@  "arg: " ^ show_pat p;
          print_endline @@ "body: " ^ show_exp rest;
          self#expression rest;
          let body = S.get () in
          S.set @@ Fun (var_name p.ppat_desc, body);        
        | [%expr [%plus [%e? t] [%e? e1] [%e? e2]]] ->          
          lift_plus t e1 e2 ~traverse:(self#expression)
        | [%expr [%lift [%e? t] [%e? e]]] ->
          let bt = bt_of_pexp_desc t.pexp_desc in
          let e' = match create_ml_expr e ~t:bt with
            | Some expr -> Expr expr
            | None -> self#expression e; S.get ()
          in          
          S.set e'
        | [%expr [%lift [%e? s] [%e? t] [%e? e]]] ->
          let bt = bt_of_pexp_desc t.pexp_desc in
          let s' = bt_of_pexp_desc s.pexp_desc in
          let e' = match create_ml_expr e ~t:bt with
            | Some expr -> Expr expr
            | None -> self#expression e; S.get ()
          in          
          S.set (Lift (s', e'))
        | [%expr if [%e? e1] < [%e? e2] then [%e? b1] else [%e? b2]] ->
          (* TODO: will not generate correctly if e1 or e2 are not lift operations *)
          self#expression e1;
          let e1' = S.get () in
          self#expression e2;
          let e2' = S.get () in
          self#expression b1;
          let b1' = S.get () in
          self#expression b2;
          let b2' = S.get () in
          (* TODO: Fix here becuase e1'=e2' *)
          let cond = Leq (e1', e2') in
          S.set (IfElse (cond, b1', b2'))          
        | _ ->
          print_endline @@ "expr: " ^ show_exp expr;
          _super#expression expr;          
    end
  in
  (* NOTE: Incorrect result since the arg_ml_expr is never updated *)
  let arg_ml_expr = match create_ml_expr arg with
    | Some expr -> expr
    | None -> failwith "You can only specialize with a constant"
  in
  let loc = to_specialize.pexp_loc in
  (* Traverse the tree inside an effect handler to collect states *)
  S.run ~init:(Expr arg_ml_expr) (fun () ->
      match to_specialize with
      (* For the function pattern, ident is the argument that is specialized. *)
      | [%expr fun [%p? ident] -> [%e? rest]] -> begin
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
