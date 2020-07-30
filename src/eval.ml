open Parser
open Ast
open Environment

let rec eval (ast : expr) : primitive =
  match ast with
  | Int(n) -> Val_Int(n)
  | Bool(b) -> Val_Bool(b)
  | Null -> Val_Null
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
  | Seq(a1, a2) -> let _ = eval a1 in eval a2
  | Block stmt -> eval stmt (* TODO add environment stacks and stuff *)
  | If(cond, a, b) -> if (as_bool @@ eval cond) then eval a else eval b (* TODO don't fail if eval_cond isn't a bool *)
  | Exit(a1) -> exit (as_int @@ eval a1)
