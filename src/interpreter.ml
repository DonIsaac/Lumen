open Parser

exception EvalError of string

type var = Var of string * int ref | Undefined

let env : var list ref = ref []
(* Evaluater *)

let is_var_defined (identifier : string) : bool = 
  let rec check_env (v_lst : var list) (id : string) : bool =
    match v_lst with
    | [] -> false
    | Var(curr, _)::t when curr = id -> true
    | _::t -> check_env t id
  in check_env !env identifier
;;

let add_var (id : string) (value : int) : int = 
  if is_var_defined id then
    raise @@ EvalError ("Identifier '" ^ id ^ "' is already defined")
  else
    let v = ref value in 
    env := Var(id, v)::(!env);
    value
;;

let modify_var (id : string) (newvalue : int) : int =
  let rec modify (v_lst : var list) : unit =
    match v_lst with
    | [] -> raise @@ EvalError ("Identifier '" ^ id ^ "' is not defined")
    | Var(_id, _value)::_ when _id = id -> _value := newvalue 
    | _::v_lst' -> modify v_lst'
  in
    modify !env;
    newvalue
;;

let var_lookup (id : string) : int = 
  let rec lookup (v_lst : var list) =
    match v_lst with
    | [] -> raise @@ EvalError ("Identifier '" ^ id ^ "' is not defined")
    | Var(_id, _value)::_ when _id = id -> !_value
    | _::v_lst' -> lookup v_lst'
  in lookup !env
;;

let rec eval (ast : expr) : int =
  match ast with
  | Int(n) -> n
  | Neg(a1) -> -(eval a1)
  | Plus(a1, a2) -> (eval a1) + (eval a2)
  | Minus(a1, a2) -> (eval a1) - (eval a2)
  | Mult(a1, a2) -> (eval a1) * (eval a2)
  | Div(a1, a2) -> (eval a1) / (eval a2)
  | Decl(id, a1) -> add_var id (eval a1) 
  | Assign(id, a1) -> modify_var id (eval a1)
  | Var(id) -> var_lookup id
  | Exit(a1) -> exit (eval a1)
