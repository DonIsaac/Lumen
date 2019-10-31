open Lexer

(* Types *)
type expr =
  | Int of int
  | Plus of expr * expr
  | Minus of expr * expr
  | Mult of expr * expr
  | Div of expr * expr
  | Exit of expr

(* Provided helper function - takes a token list and an exprected token.
 * Handles error cases and returns the tail of the list *)
let match_token (toks : token list) (tok : token) : token list =
  match toks with
  | [] -> raise (Failure(string_of_token tok))
  | h::t when h = tok -> t
  | h::_ -> raise (Failure(
      Printf.sprintf "Expected %s from input %s, got %s"
        (string_of_token tok)
        (string_of_list string_of_token toks)
        (string_of_token h)
    ))

let lookahead toks = match toks with
    h::t -> h
  | _ -> raise (Failure("Empty input to lookahead"))


(*
S -> M + S | M - S | M | exit N
M -> N * M | N / M | N
N -> n | (S)
where n is any integer
*)
(* Parses a token list. *)
let rec parser (toks : token list) : expr =
  let a1 = parse_S toks in
  let (toks, expr) = a1 in
  let next = lookahead toks in
  match next with
  | Tok_EOF -> expr
  | _ -> raise (Failure("Expected Tok_EOF, got" ^ (string_of_token next)))

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
  M -> N * M | N / M | N
*)
and parse_M (toks : token list) : (token list * expr) =
  let a1 = parse_N toks in
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

(*
  Parses the N rule.
  N -> n | -N | (S)
*)
and parse_N (toks : token list) : (token list * expr) =
  let token = lookahead toks in
  match token with
  | Tok_Int(a) -> ((match_token toks token), Int(a))(*match_token toks Tok_Int(a), Int(a)*)
  (* | Tok_Minus ->
     let a1 = parse_N (match_token toks Tok_Minus) in
     let (toks, expr) = a1 in
     (toks, I) *)

  | Tok_LParen ->
    let a1 = parse_S (match_token toks Tok_LParen) in
    let (toks, expr) = a1 in
    let tail = match_token toks Tok_RParen in
    (tail, expr)
  | _ -> raise (Failure("Unexpected token '" ^ (string_of_token token) ^ "' in parse_N"))
