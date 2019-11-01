open Lexer

exception ParseError of string

(* Types *)
type expr =
  | Int of int
  | Neg of expr
  | Plus of expr * expr
  | Minus of expr * expr
  | Mult of expr * expr
  | Div of expr * expr
  | Decl of string * expr
  | Assign of string * expr
  | Var of string
  | Exit of expr

(* Provided helper function - takes a token list and an exprected token.
 * Handles error cases and returns the tail of the list *)
let match_token (toks : token list) (tok : token) : token list =
  match toks with
  | [] -> raise (ParseError(string_of_token tok))
  | h::t when h = tok -> t
  | h::_ -> raise (ParseError(
      Printf.sprintf "Expected %s from input %s, got %s"
        (string_of_token tok)
        (string_of_list string_of_token toks)
        (string_of_token h)
    ))

let lookahead toks = match toks with
    h::t -> h
  | _ -> raise (ParseError("Empty input to lookahead"))


(*
D -> let ID = A | A | exit N
A -> ID = A | S 
S -> M + S | M - S | M 
M -> U * M | U / M | U
U -> +N | -N | N
N -> n | ID | (A)
where n is any integer
*)
(* Parses a token list. *)
let rec parser (toks : token list) : expr =
  let a1 = parse_D toks in
  let (toks, expr) = a1 in
  let next = lookahead toks in
  match next with
  | Tok_EOF -> expr
  | _ -> raise (ParseError("Expected Tok_EOF, got " ^ (string_of_token next)))

and parse_D (toks : token list) : (token list * expr) =
  let next = lookahead toks in
  match next with
  | Tok_Let ->
    let toks = match_token toks Tok_Let in
    let identifier = lookahead toks in
    (
      match identifier with
      | Tok_ID(id) ->
        let toks = match_token (match_token toks identifier) Tok_Assign in
        let a1 = parse_A toks in
        let (toks, a_expr) = a1 in
        (toks, Decl(id, a_expr))
      | _ -> raise @@ ParseError ("Unexpected token '" ^ (string_of_token identifier) ^ "', expected '" ^ (string_of_token Tok_Assign) ^ "'")
    )
  | Tok_Exit ->
    let code = parse_N (match_token toks Tok_Exit) in
    let (toks, n_expr) = code in
    (toks, Exit (n_expr))
  | _ -> parse_A toks

and parse_A (toks : token list) : (token list * expr) =
  let next = lookahead toks in
  match next with
  | Tok_ID(id) when (lookahead (match_token toks next)) = Tok_Assign ->
    let toks = match_token (match_token toks next) Tok_Assign in
    let a1 = parse_A toks in
    let (toks, a_expr) = a1 in
    (toks, Assign(id, a_expr))
  | _ -> parse_S toks
(*
  Parses the S rule.
  S -> M + S | M - S | M
*)
and parse_S (toks : token list) : (token list * expr) =
  let next = lookahead toks in
  match next with
  | Tok_Exit ->
    let code = parse_N (match_token toks Tok_Exit) in
    let (toks, n_expr) = code in
    (toks, Exit (n_expr))
  | _ ->
    let a1 = parse_M toks in
    let (toks, m_expr) = a1 in
    let next = lookahead toks in
    match next with
    | Tok_Plus ->
      let a2 = parse_S (match_token toks Tok_Plus) in
      let (toks, s_expr) = a2 in
      (toks, Plus (m_expr, s_expr))
    | Tok_Minus ->
      let a2 = parse_S (match_token toks Tok_Minus) in
      let (toks,s_expr) = a2 in
      (toks, Minus (m_expr, s_expr))
    | _ -> a1

(*
  Parses the M rule.
  M -> U * M | U / M | U
*)
and parse_M (toks : token list) : (token list * expr) =
  let a1 = parse_U toks in
  let (toks, expr) = a1 in
  let next = lookahead toks in
  match next with
  | Tok_Mult ->
    let (_tok, _expr) = parse_M (match_token toks Tok_Mult) in
    (_tok, Mult (expr, _expr))
  | Tok_Div ->
    let (_tok, _expr) = parse_M (match_token toks Tok_Div) in
    (_tok, Div (expr, _expr))
  | _ -> a1

and parse_U (toks : token list) : (token list * expr) =
  let token = lookahead toks in
  match token with
  | Tok_Plus -> parse_N (match_token toks token)
  | Tok_Minus ->
    let a1 = parse_N (match_token toks token) in
    let (toks, n_expr) = a1 in
    (toks, Neg(n_expr))
  | _ -> parse_N toks
(*
  Parses the N rule.
  N -> n | ID | (S)
*)
and parse_N (toks : token list) : (token list * expr) =
  let token = lookahead toks in
  match token with
  | Tok_Int(a) -> ((match_token toks token), Int(a))(*match_token toks Tok_Int(a), Int(a)*)
  (* | Tok_Minus ->
     let a1 = parse_N (match_token toks Tok_Minus) in
     let (toks, expr) = a1 in
     (toks, I) *)
  | Tok_ID(id) -> (match_token toks token, Var(id))
  | Tok_LParen ->
    let a1 = parse_A (match_token toks Tok_LParen) in
    let (toks, expr) = a1 in
    let tail = match_token toks Tok_RParen in
    (tail, expr)
  | _ -> raise (ParseError("Unexpected token '" ^ (string_of_token token) ^ "' in parse_N"))
