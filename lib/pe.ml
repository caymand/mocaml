open Multi_level_ops
open Ppxlib

module S = Algaeff.State.Make (struct type t = ml_ops end)    

let lift_binop t e1 e2 ~traverse ~binop =  
  let _ = traverse e1 in let e1' = S.get () in 
  let _ = traverse e2 in let e2' = S. get() in
  let e = Binop (bt_of_pexp_desc t.pexp_desc, binop (e1',  e2')) in  
  S.set e

let replace ~ident ~with_val in_op = let (let*) = Result.bind in
  (* TODO:FIXME *)
  let decrease_bt e v =
    if e.t > 0
    then {v; t = (e.t - 1)}
    else {v; t=0;} in
  
  (* TODO maybe rename to pe or partial_eval*)
  let rec eval = function
    | Binop (1, binop) ->
      eval_binop binop
    | Lift (s, e) when s <= 1 -> eval e
    | Expr e when e.t <= 1 -> begin
        match e.v with
        | Val v -> Result.ok v
        | Ident _ -> Result.error "Cannot evaluate identifiers"
      end
    | e -> Result.error @@ "Cannot eval expr of bt > 1: " ^ show_ml_ops e;  
  and eval_binop binop =
    let eval_binop' e1 e2 (oper : int -> int -> int) =
      let* v1 = eval e1 in
      let* v2 = eval e2 in
      Result.ok @@ oper v1 v2
    in
    match binop with
    | Add (e1, e2) -> eval_binop' e1 e2 Int.add
    | Sub (e1, e2) -> eval_binop' e1 e2 Int.sub
    | Mul (e1, e2) -> eval_binop' e1 e2 Int.mul
    | Div (e1, e2) -> eval_binop' e1 e2 Int.div
  in
  
  let rec go_cond cond =    
    match cond with
    | Leq (e1, e2) ->      
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
    | Binop (1, binop)->
      let (e1, e2, make_binop) = construct_binop binop in
      let* e1' = go e1 in
      let* e2' = go e2 in
      let* v = eval @@ Binop (1, make_binop (e1', e2')) in
      Result.ok @@ Expr {v=(Val v); t=0}
    | Binop (t, binop) ->
      let (e1, e2, make_binop) = construct_binop binop in
      let* e1' = go e1 in
      let* e2' = go e2 in
      if (bt_of_ops e1') = (bt_of_ops e2')
      then Result.ok @@ Binop (t-1, make_binop (e1', e2'))
      else Result.error @@ Multi_level_ops.Errors.invalid_binding_times ~e1 ~e2 ~e1' ~e2'      
    | Expr e when equal_ml_val e.v ident ->
      Result.ok @@ Expr (decrease_bt e with_val)
      (* if e.t = 1 *)
      (* then Result.ok @@ Expr (decrease_bt e with_val) *)
      (* else Result.error @@ "ICE. Invalid BT for variable" *)
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
      let* cond' = go_cond cond in
      begin
        match cond' with
        | Bool b ->
          if b then go e_then else go e_else
        | _ ->
          let* e_then' = go e_then in
          let* e_else' = go e_else in
          Result.ok @@ IfElse (cond', e_then', e_else')
      end
    | App (fn, args) ->      
      let rec go_args acc = function
        | [] -> Result.ok acc
        | (Ok arg)::tl -> go_args (arg::acc) tl
        | (Error msg)::_ -> Error msg in
      let* args' = go_args [] (List.map go args |> List.rev) in
      Result.ok @@ App (fn, args')
  in
  go in_op

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
          (* print_endline @@  "arg: " ^ show_pat p; *)
          (* print_endline @@ "body: " ^ show_exp rest; *)
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
          (* print_endline @@ "before lift " ^ show_ml_ops (S.get ()); *)
          let e' = match create_ml_expr e ~t:bt with
            | Some expr -> Expr expr
            | None -> _super#expression e; S.get () (* TODO: super or self *)
          in
          S.set e';
          (* print_endline @@ "after lift " ^ show_ml_ops (S.get ()); *)
        | [%expr [%lift [%e? s] [%e? t] [%e? e]]] ->
          let bt = bt_of_pexp_desc t.pexp_desc in
          let s' = bt_of_pexp_desc s.pexp_desc in
          let e' = match create_ml_expr e ~t:bt with
            | Some expr -> Expr expr
            | None ->
              failwith "not now"
              (* self#expression e; S.get () *)
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
        | [%expr [%app [%e? t] [%e? fn_app]]] -> begin
            match fn_app.pexp_desc with
            | Pexp_apply (fn, args) ->
              let args_op = List.map (fun (_, expr) ->
                  self#expression expr;
                  S.get () ) args
              in
              S.set @@ App (fn, args_op);
            | _ ->
              failwith "Expecte function application: [%app t fn (args)]."
          end
        | _ -> failwith @@ "Expression no implemented: " ^ Pprinter.show_exp expr;
          (* TODO: Consider making this an escape that just inserts the expression *)          
          (* TODO: Maybe also match run in here and possibly ml definitions. *)                    
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
            print_endline "LIFTET:";
            print_endline @@ show_ml_ops (S.get ());
            let specialization = replace
                ~ident:(Ident ident_loc.txt)
                ~with_val:arg_ml_expr.v
                (S.get ()) in            
            (* After specializing a function, we have to make sure that the
               arguments to the function are still in scope.*)            
            Result.fold
              ~ok:(Codegen.cogen ~loc)
              ~error:(Codegen.fail_with ~loc:rest.pexp_loc)
              specialization
          | _ -> Codegen.fail_with "invalid type" ~loc:ident.ppat_loc
        end
      | _ -> Codegen.fail_with "wrong type" ~loc:to_specialize.pexp_loc
    )
