exception IllegalExpression of string

type token =
| Tok_Int of int
| Tok_Let
| Tok_ID of string
| Tok_Assign
| Tok_Mult
| Tok_Div
| Tok_Plus
| Tok_Minus
| Tok_LParen
| Tok_RParen
| Tok_Exp
| Tok_Log
| Tok_Base
| Tok_Of
| Tok_EOF
| Tok_Exit

val lexer : string -> token list

val string_of_token : token -> string

val string_of_list : ('a -> string) -> 'a list -> string
