exception IllegalExpression of string


(** Parses an incoming string of code into a token list*)
val lexer : string -> TokenTypes.token list

val string_of_token : TokenTypes.token -> string

val string_of_list : ('a -> string) -> 'a list -> string
