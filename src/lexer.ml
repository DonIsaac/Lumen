(* Type *)
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
| Tok_EOF
| Tok_Exit

let string_of_token tok = match tok with
| Tok_Int(i) -> string_of_int i
| Tok_Let -> "let"
| Tok_ID(id) -> id
| Tok_Assign -> "="
| Tok_Mult -> "*"
| Tok_Div -> "/"
| Tok_Plus -> "+"
| Tok_Minus -> "-"
| Tok_LParen -> "("
| Tok_RParen -> ")"
| Tok_EOF -> ""
| Tok_Exit -> "exit"

let rec string_of_list conv lst =
match lst with
| [] -> ""
| h::[] -> conv h
| h::t -> (conv h) ^ " " ^ (string_of_list conv t)

exception IllegalExpression of string

let re_int = Str.regexp "[0-9]+"
let re_let = Str.regexp "let[^a-zA-Z0-9]"
let re_id = Str.regexp "[a-zA-Z][a-zA-Z0-9]*"
let re_assign = Str.regexp "="
let re_mult = Str.regexp "*"
let re_div = Str.regexp "/"
let re_plus = Str.regexp "+"
let re_minus = Str.regexp "-"
let re_lparen = Str.regexp "("
let re_rparen = Str.regexp ")"
let re_exit = Str.regexp "exit"

let re_whitespace = Str.regexp "[ \n\r\x0c\t]+"
(* Given source code returns a token list. *)
let rec lexer (input : string) : token list =
  let rec tok pos str =
    if pos >= String.length str then
      [Tok_EOF]
    else
      if (Str.string_match re_int str pos) then
        let token = Str.matched_string str in
        let tok_len = String.length token in
        (Tok_Int(int_of_string token))::(tok (pos + tok_len) str)
      else if (Str.string_match re_plus str pos) then
        Tok_Plus::(tok (pos+1) str)
      else if (Str.string_match re_minus str pos) then
        Tok_Minus::(tok (pos+1) str)
      else if (Str.string_match re_mult str pos) then
        Tok_Mult::(tok (pos+1) str)
      else if (Str.string_match re_div str pos) then
        Tok_Div::(tok (pos+1) str)
      else if (Str.string_match re_lparen str pos) then
        Tok_LParen::(tok (pos+1) str)
      else if (Str.string_match re_rparen str pos) then
        Tok_RParen::(tok (pos+1) str)
      else if (Str.string_match re_assign str pos) then
        Tok_Assign::(tok (pos+1) str)
      else if (Str.string_match re_let str pos) then
        Tok_Let::(tok (pos+3) str)
      else if (Str.string_match re_exit str pos) then
        Tok_Exit::(tok (pos+4) str)
      else if (Str.string_match re_id str pos) then
        let id = Str.matched_string str in
        let id_len = String.length id in
        (Tok_ID(id))::(tok (pos+id_len) str)
      else if (Str.string_match re_whitespace str pos) then
        let tok_len = String.length (Str.matched_string str) in
        tok (pos + tok_len) str
      else
        raise @@ IllegalExpression("Illegal token at pos " ^ (string_of_int pos))
  in
  tok 0 input
