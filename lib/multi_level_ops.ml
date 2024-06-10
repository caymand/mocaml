open Ppxlib
    
let pp_expression = Pprinter.pp_expression

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
[@@deriving show]
and ml_binop = Add of ml_ops * ml_ops
             | Sub of ml_ops * ml_ops
             | Mul of ml_ops * ml_ops
             | Div of ml_ops * ml_ops
[@@deriving show]                      
and ml_ops = Binop of int * ml_binop
           | Expr of ml_expr
           | Lift of int * ml_ops
           | Fun of string * ml_ops
           | IfElse of ml_cond * ml_ops * ml_ops
           | App of int * expression * (ml_ops list)
[@@deriving show]

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
  | Binop (t, _binop) -> t
  | Expr {v=_; t} -> t
  | Lift (s, op) -> s + bt_of_ops op
  | Fun (_, body) -> bt_of_ops body
  | _ -> failwith "Cannot find bt for this expression"

let construct_binop = function
    | Add (e1, e2) -> (e1, e2, fun (e1', e2') -> Add (e1', e2'))
    | Sub (e1, e2) -> (e1, e2, fun (e1', e2') -> Sub (e1', e2'))
    | Div (e1, e2) -> (e1, e2, fun (e1', e2') -> Div (e1', e2'))
    | Mul (e1, e2) -> (e1, e2, fun (e1', e2') -> Mul (e1', e2'))

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
