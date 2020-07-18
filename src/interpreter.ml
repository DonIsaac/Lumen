open Parser
open Ast

exception EvalError of string
exception TypeError of string

let env : (variable list ref) = ref []
(* Evaluater *)

let string_of_primitive_type (prim : primitive) : string =
  match prim with
    | Val_Int(_) -> "int"
    | Val_Bool(_) -> "bool"
let as_int (prim : primitive) : int =
  match prim with
    | Val_Int(n) -> n
    | p -> raise @@ TypeError ("Expected type int, got type " ^ string_of_primitive_type p)

let as_bool (prim : primitive) : bool =
  match prim with
    | Val_Bool(b) -> b
    | p -> raise @@ TypeError ("Expected type bool, got type " ^ string_of_primitive_type p)
let is_var_defined (identifier : string) : bool =
  let rec check_env (v_lst : variable list) (id : string) : bool =
    match v_lst with
    | [] -> false
    | Var(curr, _)::t when curr = id -> true
    | _::t -> check_env t id
  in check_env !env identifier
;;

let add_var (id : string) (value : primitive) : primitive =
  if is_var_defined id then
    raise @@ EvalError ("Identifier '" ^ id ^ "' is already defined")
  else
    let v = ref value in
    env := Var(id, v)::(!env);
    value
;;

let modify_var (id : string) (newvalue : primitive) : primitive =
  let rec modify (v_lst : variable list) : unit =
    match v_lst with
    | [] -> raise @@ EvalError ("Identifier '" ^ id ^ "' is not defined")
    | Var(_id, _value)::_ when _id = id -> _value := newvalue
    | _::v_lst' -> modify v_lst'
  in
    modify !env;
    newvalue
;;

let var_lookup (id : string) : primitive =
  let rec lookup (v_lst : variable list) =
    match v_lst with
    | [] -> raise @@ EvalError ("Identifier '" ^ id ^ "' is not defined")
    | Var(_id, _value)::_ when _id = id -> !_value
    | _::v_lst' -> lookup v_lst'
  in lookup !env
;;

let val_eq_struct a b = match a with
| Val_Int(n) ->
  (
    match b with
    | Val_Int(m) -> Val_Bool(n = m)
    | _ -> Val_Bool(false)
  )
| Val_Bool(p) ->
  (
    match b with
    | Val_Bool(q) -> Val_Bool(p = q)
    | _ -> Val_Bool(false)
  )

;;

let val_not a = match a with
| Val_Int(n) -> Val_Bool(n = 0)
| Val_Bool(p) -> Val_Bool(not p)
;;

let rec eval (ast : expr) : primitive =
  match ast with
  | Int(n) -> Val_Int(n)
  | Bool(b) -> Val_Bool(b)
  | LOr(p, q) -> Val_Bool( (as_bool @@ eval p) || (as_bool @@ eval q))
  | LAnd(p, q) -> Val_Bool( (as_bool @@ eval p) && (as_bool @@ eval q))
  | LNot(p) -> Val_Bool( not (as_bool @@ eval p))
  | StructEq(p, q) -> val_eq_struct (eval p) (eval q)
  | RefEq(p, q) -> val_eq_struct (eval p) (eval q) (* TODO implement referential equality *)
  | StructNotEq(p, q) -> val_not (val_eq_struct (eval p) (eval q))
  | RefNotEq(p, q) -> val_not (val_eq_struct (eval p) (eval q))
  | Neg(a1) -> Val_Int(- as_int (eval a1))
  | Plus(a1, a2) -> Val_Int( ( as_int @@ eval a1) + (as_int @@ eval a2) )
  | Minus(a1, a2) -> Val_Int( (as_int @@ eval a1) - (as_int @@ eval a2) )
  | Mult(a1, a2) -> Val_Int( (as_int @@ eval a1) * (as_int @@ eval a2) )
  | Div(a1, a2) -> Val_Int( (as_int @@ eval a1) / (as_int @@ eval a2) )
  | Decl(id, a1) -> add_var id (eval a1)
  | Assign(id, a1) -> modify_var id (eval a1)
  | Var(id) -> var_lookup id
  | Exp(a1, a2) -> Val_Int( int_of_float @@ (float_of_int (as_int @@ eval a1) ** (float_of_int ( as_int @@ eval a2))) )
  (* Log_b x = Log_a x/Log_a b *)
  | Log(a1, a2) -> Val_Int( int_of_float @@ log (float_of_int @@ (as_int @@ eval a2)) /. log (float_of_int @@ (as_int @@ eval a1)) )
  | Exit(a1) -> exit (as_int @@ eval a1)
