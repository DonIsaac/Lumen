open TokenTypes
exception IllegalExpression of string



let string_of_token tok = match tok with
  | Tok_Int(i)            -> string_of_int i
  | Tok_Bool(b)           -> string_of_bool b
  | Tok_ID(id)            -> id
  | Tok_Null              -> "null"
  | Tok_Term              -> ";"

  | Tok_Assign            -> "="
  | Tok_Op_LAnd           -> "and"
  | Tok_Op_LOr            -> "or"
  | Tok_Op_LNot           -> "not"
  | Tok_Op_Eq_Struct      -> "=="
  | Tok_Op_Eq_Ref         -> "==="
  | Tok_Op_NEq_Struct     -> "!="
  | Tok_Op_NEq_Ref        -> "!=="

  | Tok_Mult              -> "*"
  | Tok_Div               -> "/"
  | Tok_Plus              -> "+"
  | Tok_Minus             -> "-"
  | Tok_LParen            -> "("
  | Tok_RParen            -> ")"
  | Tok_Exp               -> "^"

  | Tok_Let               -> "let"
  | Tok_Log               -> "log"
  | Tok_Base              -> "base"
  | Tok_Of                -> "of"
  | Tok_Do                -> "do"
  | Tok_End               -> "end"
  | Tok_If                -> "if"
  | Tok_Then              -> "then"
  | Tok_Else              -> "else"
  | Tok_EOF               -> "EOF"
  | Tok_Exit              -> "exit"

let rec string_of_list conv lst = match lst with
  | []                    -> ""
  | h::[]                 -> conv h
  | h::t                  -> (conv h) ^ " " ^ (string_of_list conv t)


(* let keyword_regex k = Str.regexp (k ^ "[^0-9a-zA-Z]") *)
let keyword_regex k = Str.regexp ("\\(" ^ k ^ "\\b\\|" ^ k ^ "$\\)")

(* Primitives and Identifiers *)
let re_int                = Str.regexp "[0-9]+"
(* let re_bool               = Str.regexp "\\(true\\|false\\)\\(\b\\|$\\)" *)
let re_bool               = keyword_regex "\\(true\\|false\\)"
let re_id                 = Str.regexp "[a-zA-Z][a-zA-Z0-9]*"
let re_null               = keyword_regex "null"
let re_term               = Str.regexp ";"

(* Operators *)
let re_assign             = Str.regexp "="
let re_op_land            = keyword_regex "and"
let re_op_lor             = keyword_regex "or"
let re_op_lnot            = keyword_regex "not"
let re_op_eq_struct       = Str.regexp "=="
let re_op_eq_ref          = Str.regexp "==="
let re_op_neq_struct      = Str.regexp "!="
let re_op_neq_ref         = Str.regexp "!=="
let re_mult               = Str.regexp "*"
let re_div                = Str.regexp "/"
let re_plus               = Str.regexp "+"
let re_minus              = Str.regexp "-"
let re_lparen             = Str.regexp "("
let re_rparen             = Str.regexp ")"
let re_exp                = Str.regexp "\\^"

(* Keywords *)
let re_let                = keyword_regex "let"
let re_log                = keyword_regex "log"
let re_base               = keyword_regex "base"
let re_of                 = keyword_regex "of"
let re_do                 = keyword_regex "do"
let re_end                = keyword_regex "end"
let re_if                 = keyword_regex "if"
let re_then               = keyword_regex "then"
let re_else               = keyword_regex "else"
let re_exit               = keyword_regex "exit"

let re_whitespace         = Str.regexp "[ \n\r\x0c\t]+"

(**
  Reads in source code as a string, scans/tokenizes it, and returns
  the corresponding token list.
*)
let rec lexer (input : string) : token list =
  let rec tok pos str =
    if pos >= String.length str then
      [Tok_EOF]
    else
      if (Str.string_match re_int str pos) then
        let token = Str.matched_string str in
        let tok_len = String.length token in
        (Tok_Int(int_of_string token))::(tok (pos + tok_len) str)

      else if (Str.string_match re_bool str pos) then
        let s = Str.matched_string str in
        if s = "true" || s = "false"
          then Tok_Bool(bool_of_string @@ s)::(tok (pos+ (String.length s)) str) else raise @@ IllegalExpression ("Tried to tokenize string '" ^ s ^ "' as a boolean")

      else if (Str.string_match re_null str pos) then
        Tok_Null::(tok (pos + 4) str)

      else if (Str.string_match re_term str pos) then
        Tok_Term::(tok (pos + 1) str)

      else if (Str.string_match re_op_land str pos) then
        Tok_Op_LAnd::(tok (pos+3) str)

      else if (Str.string_match re_op_lor str pos) then
        Tok_Op_LOr::(tok (pos+2) str)

      else if (Str.string_match re_op_lnot str pos) then
        Tok_Op_LNot::(tok (pos+3) str)

      else if (Str.string_match re_op_eq_struct str pos) then
        Tok_Op_Eq_Struct::(tok (pos+2) str)

      else if (Str.string_match re_op_eq_ref str pos) then
        Tok_Op_Eq_Ref::(tok (pos+3) str)

      else if (Str.string_match re_op_neq_struct str pos) then
        Tok_Op_NEq_Struct::(tok (pos+2) str)

      else if (Str.string_match re_op_neq_ref str pos) then
        Tok_Op_NEq_Ref::(tok (pos+3) str)

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

      else if (Str.string_match re_exp str pos) then
        Tok_Exp::(tok (pos+1) str)

      else if (Str.string_match re_log str pos) then
        Tok_Log::(tok (pos+3) str)

      else if (Str.string_match re_base str pos) then
        Tok_Base::(tok (pos+4) str)

      else if (Str.string_match re_of str pos) then
        Tok_Of::(tok (pos+2) str)

      else if (Str.string_match re_do str pos) then
        Tok_Do::(tok (pos+2) str)

      else if (Str.string_match re_end str pos) then
        Tok_End::(tok (pos+3) str)

      else if (Str.string_match re_if str pos) then
        Tok_If::(tok (pos+2) str)

      else if (Str.string_match re_then str pos) then
        Tok_Then::(tok (pos+4) str)

      else if (Str.string_match re_else str pos) then
        Tok_Else::(tok (pos+4) str)

      else if (Str.string_match re_exit str pos) then
        Tok_Exit::(tok (pos+4) str)

      (* Tok_ID must be last, otherwise it will match keywords *)
      else if (Str.string_match re_id str pos) then
        let id = Str.matched_string str in
        let id_len = String.length id in
        (Tok_ID(id))::(tok (pos+id_len) str)

      else if (Str.string_match re_whitespace str pos) then
        let tok_len = String.length (Str.matched_string str) in
        tok (pos + tok_len) str

      else
        raise @@ IllegalExpression("Illegal token " ^ (String.make 1 str.[pos]) ^ " at pos " ^ (string_of_int pos))
  in
  tok 0 input
