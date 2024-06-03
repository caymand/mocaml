open Ppxlib

let fail_with text ~loc =
  Location.raise_errorf ~loc text

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
type ml_val = Val of int | Ident of string [@@deriving show]
(* Wrapper to store binding time info *)
and ml_expr = {
  v: ml_val;
  t: int;
} [@@deriving show]

(* Supported Ops *)
type ml_ops = Add of int * ml_ops * ml_ops | Expr of ml_expr [@@deriving show]

module E = Algaeff.State.Make (struct type t = (string * int) list end)

let eval_leaf = function
  | Val v -> Some v
  | Ident ident ->
    List.assoc_opt ident @@ E.get ()

let rec eval e = let (let*) = Result.bind in
  match e with
  | Add (1, Expr {v=v1; t=1}, Expr {v=v2; t=1}) ->
    let v = Option.bind (eval_leaf v1)
        (fun v1' -> Option.bind (eval_leaf v2) (
             fun v2' ->
               Some (v1'+v2'))) in
    if Option.is_none v then Result.error "Variable out of scope used"
    else let v' = Option.get v in
      Result.ok @@ Expr {v=(Val v'); t=0}
  | Add (t, Expr e1, Expr e2) ->
    if e1.t != e2.t || e1.t != t
    then Result.error "Cannot add number of different binding times."
    else let t' = t-1 in
      Result.ok (Add (t', Expr {e1 with t = t'}, Expr {e2 with t = t'}))
  | Add (t, e1, e2) ->
    let* e1' = eval e1 in
    let* e2' = eval e2 in
    Result.ok @@ Add (t-1, e1', e2')
  | Expr e -> Result.ok @@ Expr e
  (* | _ -> Result.error "Not implemented" *)

let specialize_ml_ops expr arg _arg_name =
  (* First, fully evaluate the argument *)
  let v = eval arg in
  Result.get_ok v |> show_ml_ops |> print_endline;

  expr

let get_bt = function
  | Pexp_constant (Pconst_integer (s, _)) -> int_of_string s
  | _ ->
    print_endline "Expected constant of the bt";
    failwith "bt must be a constant"

let create_ml_expr ?(t = 0) (pexp_desc : expression_desc) =
  match pexp_desc with
  | Pexp_constant (Pconst_integer (s, _)) ->
    let v = Val (int_of_string s) in
    Expr {v; t}
  | Pexp_ident {txt =(Lident var); loc=_loc} ->
    Expr {v=(Ident var); t}
  | _ ->
    failwith "not implemented here"

let specialize (to_specialize : expression) (arg : expression) : expression =  
  let mapper = object

    inherit [ml_ops] Ast_traverse.lift as super

    method string _e = Expr {v=(Val 0); t=0}
    method tuple _ = failwith "tuple not implemented"
    method unit _ = failwith "not implemented"
    method record _ = failwith "not implemented"
    method nativint _ = failwith "nativeint not implemented"
    method int64 _ = failwith "int64 not implemented"
    method int32 _ = failwith "not implemented"
    method other _ = failwith "not implemented"
    method nativeint _ = failwith "not implemented"
    method int i =
      print_int i;
      failwith "int not implemented"
    method float _ =  failwith "not implemented"
    method constr _c ml_ops = List.hd ml_ops
      (* print_endline @@ "constr: " ^ c; *)
      (* print_endline @@ [%show: ml_ops list] _foo; *)
      (* failwith "constr not implemented" *)
      
    method char _ = failwith "char not implemented"
    method bool _ = failwith "not implemented"
    method array _ = failwith "not implemented"

    method! extension ext =
      let (ext_loc, payload) = ext in
      let _loc = ext_loc.loc in
      match payload with
      | PPat (_pat, Some _expr) ->
        print_endline "ppat";
        super#extension ext
      | PStr strct -> begin
          "struct " ^ ext_loc.txt ^ " "  ^ (show_strct strct) |> print_endline;
          match ext_loc.txt with
          | "plus" -> begin
              match strct with
              | [%str [%e? t] [%e? e1] [%e? e2]] ->
                print_endline @@ "e1: " ^ (show_exp e1);
                let e1' = super#expression e1 in
                show_ml_ops e1' |> print_endline;
                print_endline @@ "e2: " ^ (show_exp e2);
                let e2' = super#expression e2 in
                show_ml_ops e2' |> print_endline;
                Add (get_bt t.pexp_desc, e1',  e2')
              | _ ->                
                failwith "Incorrect format"
            end
          | "lift" -> begin
              match strct with
              | [%str [%e? t] [%e? e]] ->
                print_endline "matched lift";
                let bt = get_bt t.pexp_desc in                
                create_ml_expr e.pexp_desc ~t:bt
              | _ -> failwith "incorrect format"
            end
          | _ ->
            print_endline "huh";
            failwith "extension not yet implemented"
        end
      | _ ->
        (* fail_with "ICE. Case should not happen" ~loc *)
        super#extension ext

    method! expression expr =
      "expr: " ^ (show_exp expr) |> print_endline;
      match expr with
      | [%expr fun [%p? _] -> [%e? rest]] ->
        (* We ignore these bindings.
           They will have to be specizlized later *)
        super#expression rest
      | _ -> begin
          match expr.pexp_desc with
          | Pexp_constant (Pconst_integer (s, _)) ->
            let v = int_of_string s in
            Expr {v=(Val v); t = 0}
          | _ -> failwith "not implemented here"
        end
  end in  
  let _arg' = mapper#expression arg in
  let loc = to_specialize.pexp_loc in
  match to_specialize with
  | [%expr fun [%p? ident] -> [%e? rest]] -> begin
      match ident.ppat_desc with
      | Ppat_var _ident_loc ->        
        let _to_specialize' = mapper#expression rest in
        show_ml_ops _to_specialize' |> print_endline;
        [%expr fun [%p ident] -> [%e rest]]
      | _ -> fail_with "invalid type" ~loc:ident.ppat_loc
    end
  | _ -> fail_with "wrong type" ~loc:to_specialize.pexp_loc


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
