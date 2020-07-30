open Ast

let env : (variable list ref) = ref []

let string_of_primitive_type (prim : primitive) : string =
  match prim with
    | Val_Int(_) -> "int"
    | Val_Bool(_) -> "bool"
    | Val_Null -> "null"
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
| Val_Null -> Val_Bool(b = Val_Null)

;;

let val_not a = match a with
| Val_Int(n)  -> Val_Bool(n = 0)
| Val_Bool(p) -> Val_Bool(not p)
| Val_Null    -> Val_Bool(true)
;;
