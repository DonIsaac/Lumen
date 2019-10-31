exception EvalError of string

val eval : Parser.expr -> int
