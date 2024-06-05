open Ppxlib

let fail_with text ~loc =
  Location.raise_errorf ~loc "%s" text

let pp_pattern fmt pat =
  match pat.ppat_desc with
  | Ppat_var loc -> print_endline loc.txt;
  | _ -> ();
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
type ml_ops = Add of int * ml_ops * ml_ops
            | Expr of ml_expr
            | Lift of ml_ops [@@deriving show, eq]
                

module E = Algaeff.State.Make (struct type t = (string * int) list end)

let eval_leaf = function
  | Val v -> Some v
  | Ident ident ->
    List.assoc_opt ident @@ E.get ()

let rec bt_of_ops = function
  | Add (t, _, _) -> t
  | Expr {v=_; t} -> t
  | Lift op -> 1 + bt_of_ops op

let replace ~ident ~with_val in_op = let (let*) = Result.bind in
  let rec go op = 
    match op with
    | Add (1, Expr {v=v1; t=1}, Expr {v=v2; t=1}) ->
      let v = Option.bind (eval_leaf v1)
          (fun v1' -> Option.bind (eval_leaf v2) (
               fun v2' ->
                 Some (v1'+v2'))) in
      if Option.is_none v then Result.error "Variable out of scope used"
      else let v' = Option.get v in
        Result.ok @@ Expr {v=(Val v'); t=0}
    | Add (t, Expr e1, Expr e2) when e1.t != e2.t || e1.t != t ->
      Result.error @@ Printf.sprintf
        "Cannot add number of different binding times: %d, %d, %d"
        t e1.t e2.t
    | Add (_, e1, e2) ->
      let* e1' = go e1 in
      let* e2' = go e2 in
      if (bt_of_ops e1') = (bt_of_ops e2') 
      then Result.ok @@ Add (bt_of_ops e1', e1', e2')
      else Result.error @@
        Printf.sprintf {|ICE. Binding times did not match. Before:
%s
%s
After:
%s
%s
 |}(show_ml_ops e1) (show_ml_ops e2) (show_ml_ops e1') (show_ml_ops e2')
    | Expr e when equal_ml_val e.v ident ->
      Result.ok @@ Lift (Expr {v=with_val; t = (e.t - 1)})
    | Expr e -> Result.ok @@ Expr e
    | _ -> Result.ok op    
  in
  go in_op

let gen_plus ~ctxt expr =
  let loc = Expansion_context.Extension.extension_point_loc ctxt in
  match expr with
  | [%expr [%e? t] [%e? e1] [%e? e2]] ->
    begin
      match t with
      | [%expr 1] -> [%expr [%e e1] + [%e e2]]
      (* TODO: Add case for other binding times *)
      | _ -> fail_with "Invalid binding time" ~loc
    end
  | _ -> fail_with "failed generating code for plus" ~loc


let rec cogen ~loc = function
  | Add (_t, e1, e2) ->
    let e1' = cogen e1 ~loc
    and e2' = cogen e2 ~loc in
    [%expr [%e e1'] [%e e2']]
  | Expr e -> begin
      match e.v with
      | Val v ->      
        [%expr [%e (Ast_builder.Default.eint v ~loc)]]
      | Ident id -> [%expr [%e (Ast_builder.Default.evar id ~loc)]]
    end
  | Lift e -> cogen e ~loc

let bt_of_pexp_desc = function
  | Pexp_constant (Pconst_integer (s, _)) -> int_of_string s
  | _ ->
    print_endline "Expected constant of the bt";
    failwith "
bt must be a constant"

let create_ml_expr ?(t = 0) (pexp_desc : expression_desc) =
  match pexp_desc with
  | Pexp_constant (Pconst_integer (s, _)) ->
    let v = Val (int_of_string s) in
    {v; t}
  | Pexp_ident {txt =(Lident var); loc=_loc} ->
    {v=(Ident var); t}
  | _ ->    
    failwith "not implemented here"

let specialize (to_specialize : expression) (arg : expression) : expression =
  let module S = Algaeff.State.Make (struct type t = ml_ops end) in
  (* let (let*\) = Result.bind in *)
  let lift v ident = object           
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
                (* let op =  *)
                let _ = super#expression e1 in let e1' = S.get () in 
                let _ = super#expression e2 in let e2' = S.get() in
                S.set @@ Add (bt_of_pexp_desc t.pexp_desc, e1',  e2')
                (*   let* e1'' = replace ~ident:ident ~with_val:v e1' in *)
                (*   let* e2'' = replace ~ident:ident ~with_val:v e2' in                   *)
                (*   Result.ok @@ Add (bt_of_pexp_desc t.pexp_desc, e1'',  e2'') *)
                (* in *)
                (* let _op' = Result.fold *)
                (*   ~ok:(fun op -> cogen op ~loc) *)
                (*   ~error:(fun err -> fail_with err ~loc) *)
                (*   op *)
                (* in () *)
              | _ ->                
                failwith "Incorrect format"
            end
          | "lift" -> begin
              match strct with
              | [%str [%e? t] [%e? e]] ->
                let bt = bt_of_pexp_desc t.pexp_desc in                
                let ml_expr = create_ml_expr e.pexp_desc ~t:bt in
                S.set (Expr ml_expr)
              | _ -> failwith "incorrect format"
            end
          | _ ->
            failwith "extension not yet implemented"
        end
      | _ ->
        super#extension ext

    (* Traverse expressions and build up simples values *)
    method! expression expr =
      match expr with
      | [%expr fun [%p? _] -> [%e? rest]] ->
        (* We ignore these bindings.
           They will have to be specizlized later *)
        super#expression rest          
      (* TODO: more cases should be handled *)
      | _ -> begin
          match expr.pexp_desc with
          | Pexp_constant (Pconst_integer (s, _)) ->
            let v = int_of_string s in
            S.set @@ Expr {v=(Val v); t = 0}          
          | _ ->
            print_endline @@ "unexpected: " ^ show_exp expr;
            failwith "not implemented here"
        end
  end in
  let arg_ml_expr = create_ml_expr arg.pexp_desc in
  let loc = to_specialize.pexp_loc in
  (* Traverse the tree inside an effect handler to collect states *)
  S.run ~init:(Expr arg_ml_expr) (fun () ->
      match to_specialize with
      | [%expr fun [%p? ident] -> [%e? rest]] -> begin
          match ident.ppat_desc with
          | Ppat_var ident_loc ->        
            (lift arg_ml_expr.v @@ Ident ident_loc.txt)#expression rest;
            let to_specialize' = S.get ()
                                 |> replace ~ident:(Ident ident_loc.txt) ~with_val:arg_ml_expr.v
            in
            begin match to_specialize' with
              | Ok res ->
                show_ml_ops res |> print_endline;
                [%expr fun [%p ident] -> [%e rest]]
              | Error err -> fail_with err ~loc:rest.pexp_loc
            end
          | _ -> fail_with "invalid type" ~loc:ident.ppat_loc
        end
      | _ -> fail_with "wrong type" ~loc:to_specialize.pexp_loc
    )

let map_structure (structure : structure) =
  (* TODO: maybe inpalce updates. We already track these effects *)
  let module E = Algaeff.State.Make (struct type t = ml_defs list end) in

  let mapper = object
    inherit Ast_traverse.map as super

    method! structure_item stri =
      let loc = stri.pstr_loc in
      match stri with
      | [%stri [%%ml let [%p? decl]  = [%e? expr]]] -> begin
          match decl.ppat_desc with
          | Ppat_var loc' ->
            let fname = loc'.txt in
            let def = { name = fname; expr = expr } in
            E.modify (fun defs -> def::defs);
            (* Extract the underlying function. *)
            [%stri let [%p decl]  = failwith "You cannot use this function. Specialize it first"]
          | _ -> fail_with "invalid multi level declaration" ~loc
        end
      | [%stri let [%p? lhs] = [%run [%e? f] [%e? expr]]] -> begin
          match f.pexp_desc with
          | Pexp_ident ident ->
            let fname = match ident.txt with | Lident fname -> fname | _ -> "" in
            begin
              match List.find_opt
                      (fun def -> String.equal fname def.name)
                      (E.get ())
              with
              | Some ml_def ->
                print_endline @@ "specializing " ^ (show_ml_defs ml_def);
                let expr' = specialize ml_def.expr expr in
                [%stri let [%p lhs] = [%e expr']]
              | None ->
                (* TODO: Write function name *)
                fail_with "multi level function not found" ~loc
            end             
          | _ -> fail_with "run not impl for more than 1 argument to specializer" ~loc
        end
      | _ -> super#structure_item stri
  end
  in
  E.run ~init:[] (fun () -> mapper#structure structure)
