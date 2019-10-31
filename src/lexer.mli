type token =
| Tok_Int of int
| Tok_Mult
| Tok_Div
| Tok_Plus
| Tok_Minus
| Tok_LParen
| Tok_RParen
| Tok_EOF
| Tok_Exit

val lexer : string -> token list

val string_of_token : token -> string

val string_of_list : ('a -> string) -> 'a list -> string
