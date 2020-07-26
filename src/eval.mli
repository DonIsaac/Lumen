open Ast
exception EvalError of string
exception TypeError of string

(* type primitive = Val_Int of int | Val_Bool of bool *)
val eval : Ast.expr -> primitive
