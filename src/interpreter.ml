open Parser

(* Evaluater *)

let rec eval (ast : expr) : int =
  match ast with
  | Int(n) -> n
  | Plus(a1, a2) -> (eval a1) + (eval a2)
  | Minus(a1, a2) -> (eval a1) - (eval a2)
  | Mult(a1, a2) -> (eval a1) * (eval a2)
  | Div(a1, a2) -> (eval a1) / (eval a2)
  | Exit(a1) -> exit (eval a1)
