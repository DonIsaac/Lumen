exception ParseError of string

type expr =
  | Int of int
  | Plus of expr * expr
  | Minus of expr * expr
  | Mult of expr * expr
  | Div of expr * expr
  | Decl of string * expr
  | Assign of string * expr
  | Var of string
  | Exit of expr

val parser : Lexer.token list -> expr
val parse_S : Lexer.token list -> Lexer.token list * expr
val parse_M : Lexer.token list -> Lexer.token list * expr
val parse_N : Lexer.token list -> Lexer.token list * expr
