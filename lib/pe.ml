open Multi_level_ops
open Ppxlib
open! Base
open! Stdio
    
module S = Algaeff.State.Make (struct type t = ml_ops end)

(* TODO: This module should be about done.
   The only thing left to check is that all the bt rules fir
   type checking is implemented. *)

let lift_binop t e1 e2 ~traverse ~binop =  
  let _ = traverse e1 in let e1' = S.get () in 
  let _ = traverse e2 in let e2' = S. get() in
  let e = Binop (bt_of_pexp_desc t.pexp_desc, binop (e1',  e2')) in  
  S.set e

let decrease_bt e v =
  if e.t > 0
  then {v; t = (e.t - 1)}
  else {v; t=0;} 

let rec fun_body fn = match fn with
  | Fun (_, body) -> fun_body body
  | body -> body 

let rec replace ~ident ~with_val in_op =

  let (let*) = Result.(>>=) in
  let (>>=) = Result.(>>=) in

  let module R = Algaeff.Reader.Make (Bool) in

  let rec eval = function
    | Binop (1, binop) ->
      eval_binop binop
    | Lift (s, e) when s <= 1 -> eval e
    | Expr e when e.t <= 1 -> begin
        match e.v with
        | Val v -> Ok v        
        | ident' when equal_ml_val ident' ident  -> begin
            match with_val with
            | Ident _ -> Error "Impossible"
            | Val v -> Ok v
          end
        | _ -> Error ("Cannot evaluate identifiers: " ^ show_ml_val e.v)
      end
    | e -> Error ("Cannot eval expr of bt > 1: " ^ show_ml_ops e)
  and eval_binop binop =
    let eval_binop' e1 e2 (oper : int -> int -> int) =
      let* v1 = eval e1 in
      let* v2 = eval e2 in
      Ok (oper v1 v2)
    in
    match binop with
    | Add (e1, e2) -> eval_binop' e1 e2 Int.(+)
    | Sub (e1, e2) -> eval_binop' e1 e2 Int.(-)
    | Mul (e1, e2) -> eval_binop' e1 e2 Int.( * )
    | Div (e1, e2) -> eval_binop' e1 e2 Int.(/)
  in

  let rec go_cond cond =    
    match cond with
    | Leq (e1, e2) ->      
      let* e1' = go e1 in
      let* e2' = go e2 in      
      if bt_of_ops e1' = bt_of_ops e2' && bt_of_ops e1' = 0
      then let* v1 = eval e1' in
        let* v2 = eval e2' in
        Ok (Bool (v1 < v2))
      else Ok (Leq (e1', e2'))
    | b -> Ok b  
  and go op =    
    match op with
    | Binop (1, binop)->
      let (e1, e2, make_binop) = construct_binop binop in
      let* e1' = go e1 in
      let* e2' = go e2 in      
      let* v = eval @@ Binop (1, make_binop (e1', e2')) in      
      Ok (Expr {v=(Val v); t=0})
    | Binop (t, binop) ->
      let (e1, e2, make_binop) = construct_binop binop in
      let* e1' = go e1 in
      let* e2' = go e2 in
      if (bt_of_ops e1') = (bt_of_ops e2')
      then Ok (Binop (t-1, make_binop (e1', e2')))
      else Error(        
          Multi_level_ops.Errors.invalid_binding_times ~e1 ~e2 ~e1' ~e2')
    | Expr e when equal_ml_val e.v ident ->
      Ok (Expr (decrease_bt e with_val))    
    | Expr e ->
      Ok (Expr (decrease_bt e e.v))
    | Fun (a, body) ->
      let* body' = go body in
      Ok (Fun (a, body'))
    (* Release the inner value. s specializations has occured. *)
    | Lift (1, e) when bt_of_ops e = 0 ->
      let* e' = go e in
      Ok e'
    | Lift (s, e) ->
      (* In this case, if t=0 and s=0 the expression is just evaluated as is *)
      let t = bt_of_ops e in
      let* e' = go e in
      if t = 0
      then Ok (Lift (s-1, e'))
      else Ok (Lift (s, e'))
    | IfElse (cond, e_then, e_else) when bt_of_ops e_then = bt_of_ops e_else->
      let* cond' = go_cond cond in
      begin
        match cond' with
        | Bool b -> R.scope (fun _ -> true) @@ fun () ->
          if b
          then go e_then
          else go e_else
        | _ ->
          let* e_then' = go e_then in
          let* e_else' = go e_else in
          Ok (IfElse (cond', e_then', e_else'))
      end
    | IfElse _ -> Error "Branches must have the same binding times"
    (* Test of the first argument got smaller.
       Only also works for static tests also. *)
    | App (t, fn, arg::args) -> begin
        match bt_of_ops arg with
        | 1 ->        
          let* v = eval arg >>= (fun v -> Result.return @@ Val v) in
          if Multi_level_ops.equal_ml_val v with_val
          then
            let body = fun_body in_op in
            let is_static_if = R.read () in
            let* args' = Result.all @@ List.map ~f:go args in
            if is_static_if
            then replace ~ident ~with_val:v body
            else Result.return @@ App (t - 1, fn, args) (* TODO: Safe to remove specialized arg? *)
          else Result.fail "Possibility for infinite recursion detected."
        | _ ->
          Result.fail @@
          "First argument to multi-level function should of bt=1.\nExpr: " ^ Multi_level_ops.show_ml_ops op
      end
    | App (_, _, []) -> Result.fail "App must take at least 1 argument"
  in  
  R.run ~env:false @@ fun () -> go in_op

let specialize (to_specialize : expression) (arg : expression) : expression =  
  let lift v ident =
    object (self)        
      inherit Ast_traverse.iter as _super      
      (* Traverse expressions that we try to specialize.
         The expression will either be a function, where we try to specialize
         one of the arguments, or a multilevel expression. The later happens if there
         is only one argument left to sepcialize.*)
      method! expression expr =
        match expr with
        | [%expr fun [%p? p] -> [%e? rest]] ->
          self#expression rest;
          let body = S.get () in
          S.set @@ Fun (var_name p.ppat_desc, body);        
        | [%expr [%add [%e? t] [%e? e1] [%e? e2]]] ->          
          lift_binop t e1 e2
            ~traverse:(self#expression)
            ~binop:(fun (e1', e2') -> Add (e1', e2'))
        | [%expr [%sub [%e? t] [%e? e1] [%e? e2]]] ->          
          lift_binop t e1 e2
            ~traverse:(self#expression)
            ~binop:(fun (e1', e2') -> Sub (e1', e2'))
        | [%expr [%mul [%e? t] [%e? e1] [%e? e2]]] ->          
          lift_binop t e1 e2
            ~traverse:(self#expression)
            ~binop:(fun (e1', e2') -> Mul (e1', e2'))
        | [%expr [%div [%e? t] [%e? e1] [%e? e2]]] ->          
          lift_binop t e1 e2
            ~traverse:(self#expression)
            ~binop:(fun (e1', e2') -> Div (e1', e2'))
        | [%expr [%lift [%e? t] [%e? e]]] ->
          let bt = bt_of_pexp_desc t.pexp_desc in
          (* Try to lift either constant or ident.
             In case that fails, recursively lift the sub expression*)
          let e' = match create_ml_expr e ~t:bt with
            | Some expr -> Expr expr
            | None -> _super#expression e; S.get () (* TODO: super or self *)
          in
          S.set e';
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
          let e_then = S.get () in          
          self#expression b2;
          let e_else = S.get () in
          let cond = Leq (e1', e2') in
          S.set (IfElse (cond, e_then, e_else))        
        (* The last case can be any OCaml expression. However thse might
           still possibly contain ML ops.*)        
        | [%expr [%app [%e? t] [%e? fn_app]]] ->
          begin
            match fn_app.pexp_desc with
            | Pexp_apply (
                { pexp_desc = Pexp_ident { txt = Lident fname; _ }; _ },
                args) ->
              let bt = bt_of_pexp_desc t.pexp_desc
              and args_op = List.map args ~f:(fun (_, expr) ->
                  self#expression expr;
                  S.get ()) 
              in
              S.set @@ App (bt, fname, args_op);
            | _ ->
              failwith "Expected function application: [%app t fn (args)]."
          end
        | _ -> failwith @@
          "Expression not implemented: " ^ (Pprintast.string_of_expression expr);                    
    end
  in
  let arg_ml_expr = match create_ml_expr arg with
    | Some expr -> expr
    | None -> failwith "You can only specialize with a constant"
  in
  (* Specialize a: fun args -> body.
     NOTE: Body might itself be a fun.*)  
  let loc = to_specialize.pexp_loc in
  (* Specialize a: fun args -> body.
     NOTE: Body might itself be a fun.*)
  let specialize_fun arg body =
    match arg.ppat_desc with
    | Ppat_var ident_loc ->
      let lift_body = lift arg_ml_expr.v (Ident ident_loc.txt) in
      lift_body#expression body;
      let specialization = replace
          ~ident:(Ident ident_loc.txt)
          ~with_val:arg_ml_expr.v
          (S.get ()) in
      begin
        match specialization with
        | Ok specialization -> Codegen.cogen ~loc specialization
        | Error msg -> Codegen.fail_with ~loc:body.pexp_loc msg
      end        
    | _ -> Codegen.fail_with "invalid type" ~loc:arg.ppat_loc
  in
  (* Traverse the tree inside an effect handler to collect states *)
  S.run ~init:(Expr arg_ml_expr) (fun () ->
      (* Codegen.fail_with ~loc:to_specialize.pexp_loc "foo" *)
      print_endline "to specialize:";
      print_endline (Pprinter.show_exp to_specialize);
      match to_specialize with
      | [%expr
        let rec [%p? fn] = fun [%p? arg] -> [%e? body]
        in [%e? _]] ->
        specialize_fun arg body
      | [%expr fun [%p? arg] -> [%e? body]] ->
        specialize_fun arg body
      | _ -> Codegen.fail_with ~loc:to_specialize.pexp_loc @@
        "wrong type: " ^ (Pprinter.show_exp to_specialize)
    )

(* begin *)
(*          match ident.ppat_desc with *)
(*          | Ppat_var ident_loc -> *)
(*            let lift_body = lift arg_ml_expr.v (Ident ident_loc.txt) in *)
(*            lift_body#expression rest; *)
(*            let specialization = replace *)
(*                ~ident:(Ident ident_loc.txt) *)
(*                ~with_val:arg_ml_expr.v *)
(*                (S.get ()) in *)
(*            Result.fold *)
(*              ~ok:(Codegen.cogen ~loc ~fname:ident) *)
(*              ~error:(Codegen.fail_with ~loc:rest.pexp_loc) *)
(*              specialization *)
(*          | _ -> Codegen.fail_with "invalid type" ~loc:ident.ppat_loc *)
(*        end *)
