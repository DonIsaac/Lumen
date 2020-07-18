(**
  parser.ml - a LALR(1) parser.

  The following is the CFG this parser accepts in psudo-made-up-BNF

  PROG -> STMT_CHAIN "EOF"
  TERM -> ";" | "\n" (* TODO Should newline act as a terminator? *)

  (* Statements and loops
  ==================================== *)
  STMT_CHAIN -> STMT_BLOCK STMT_BLOCK | STMT_BLOCK
  STMT_BLOCK -> "do" STMT_CHAIN "end" | STMT
  STMT ->       STMT_CTL | EXPR TERM
  STMT_CTL ->   STMT_IF       (* TODO add more control statements and loops *)
  STMT_IF ->    "if" STMT_BLOCK "then" STMT_BLOCK ["else" STMT_BLOCK]

  STMT ->
  (* Variable assignment and declaration
  ==================================== *)
  EXPR -> D
  D -> let ID = A | A | "exit" N
  A -> ID = A | S

  (* TODO block statements should go here *)

  (* Mathematical operations
  ==================================== *)
  EXPR_LOR -> EXPR_LAND | EXPR_LAND "or" EXPR_LOR
  EXPR_LAND -> EXPR_EQ | EXPR_EQ "and" EXPR_LAND
  EXPR_EQ -> EXPR_ADD | EXPR_ADD OP_EQ EXPR_EQ
    OP_EQ -> "==" | "===" | "!=" | "!=="
  EXPR_ADD -> M + EXPR_ADD | M - EXPR_ADD | M
  M -> E * M | E / M | E
  E -> U ^ E | "log base" A "of" U | U
  U -> "+"N | "-"N | N
  N -> n | ID | BOOL | (A)
  BOOL -> "true" | "false"
  where n is any integer
*)
open TokenTypes
open Lexer
open Ast
exception ParseError of string

(** Provided helper function - takes a token list and an expected token.
    If the head of the token list is equal to the expected token, then the head
    is popped off and the tail is returned. Otherwise, a ParseError is raised. *)
let match_token (toks : token list) (tok : token) : token list =
  match toks with
  | []                -> raise (ParseError(Printf.sprintf "Unexpected end of input, expected token %s" (string_of_token tok)))
  | h::t when h = tok -> t
  | h::_              -> raise (ParseError(
      Printf.sprintf "Unexpected token %s, expected %s from input %s"
        (string_of_token h)                   (* actual   *)
        (string_of_token tok)                 (* expected *)
        (string_of_list string_of_token toks)
    ))

let lookahead toks = match toks with
    h::t -> h
  | _ -> raise (ParseError("Empty input to lookahead"))


let rec parser (toks : token list) : expr =
(** Parses a token list. *)
  let a1 = parse_D toks in
  let (toks, expr) = a1 in
  let next = lookahead toks in
  match next with
  | Tok_EOF -> expr
  | _ -> raise (ParseError("Expected Tok_EOF, got " ^ (string_of_token next)))
(** Parses the D rule
    D -> let ID = A | A | "exit" N | "exit" *)
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
    let toks = match_token toks Tok_Exit in
    (* Check for an exit code, default to 0 if one isn't provided *)
    let code =
      if (lookahead toks) = Tok_EOF then
        Int 0
      else (
        let a2 = parse_N toks in
        let (toks, n_expr) = a2 in
        n_expr
      ) in
    (toks, Exit code)
  | _ -> parse_A toks

and parse_A (toks : token list) : (token list * expr) =
  let next = lookahead toks in
  match next with
  | Tok_ID(id) when (lookahead (match_token toks next)) = Tok_Assign ->
    let toks = match_token (match_token toks next) Tok_Assign in
    let a1 = parse_A toks in
    let (toks, a_expr) = a1 in
    (toks, Assign(id, a_expr))
  | _ -> parse_expr_lor toks

and parse_expr_lor (toks : token list) : (token list * expr) =
  let a1 = parse_expr_land toks in
  let (toks, expr_land) = a1 in
  match lookahead toks with
  | Tok_Op_LOr ->
      let a2 = parse_expr_lor (match_token toks Tok_Op_LOr) in
      let (toks, expr_lor) = a2 in
      (toks, LOr(expr_land, expr_lor))
  | _ -> a1

and parse_expr_land (toks : token list) : (token list * expr) =
  let a1 = parse_expr_eq toks in
  let (toks, expr_eq) = a1 in
  match lookahead toks with
  | Tok_Op_LAnd ->
    let a2 = parse_expr_land (match_token toks Tok_Op_LAnd) in
    let (toks, expr_land) = a2 in
    (toks, LAnd(expr_eq, expr_land))
  | _ -> a1

and parse_expr_eq (toks : token list) : (token list * expr) =
  let a1 = parse_S toks in
  let (toks, expr_add) = a1 in
  match lookahead toks with
  | Tok_Op_Eq_Struct  ->
      let a2 = parse_S (match_token toks Tok_Op_Eq_Struct) in
      let (toks, expr_s) = a2 in
      (toks, StructEq(expr_add, expr_s))
  | Tok_Op_Eq_Ref     ->
      let a2 = parse_S (match_token toks Tok_Op_Eq_Ref) in
      let (toks, expr_s) = a2 in
      (toks, RefEq(expr_add, expr_s))
  | Tok_Op_NEq_Struct ->
      let a2 = parse_S (match_token toks Tok_Op_NEq_Struct) in
      let (toks, expr_s) = a2 in
      (toks, StructNotEq(expr_add, expr_s))
  | Tok_Op_NEq_Ref    ->
      let a2 = parse_S (match_token toks Tok_Op_NEq_Ref) in
      let (toks, expr_s) = a2 in
      (toks, RefNotEq(expr_add, expr_s))
  | _ -> a1
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
  M -> E * M | E / M | E
*)
and parse_M (toks : token list) : (token list * expr) =
  let a1 = parse_E toks in
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
  Parses the E rule.
  OLD: E -> U ^ E | log base A of U | U
  NEW: E -> U ^ E | log base A of A | U
*)
and parse_E (toks : token list) : (token list * expr) =
  let next = lookahead toks in
  match next with
  | Tok_Log -> (* Parse log expressions *)
    let toks = match_token ( match_token toks Tok_Log) Tok_Base in
    let a1 = parse_A toks in
    let (toks, a_expr) = a1 in
    let a2 = parse_A ( match_token toks Tok_Of) in
    let (toks, a_expr') = a2 in
    (toks, Log(a_expr, a_expr'))
  | _ ->  (* Parse exp or U rules *)
    let a1 = parse_not toks in
    let (toks, not_expr) = a1 in
    let next = lookahead toks in
    match next with
    | Tok_Exp ->
      let a2 = parse_E (match_token toks Tok_Exp) in
      let (toks, e_expr) = a2 in
      (toks, Exp(not_expr, e_expr))
    | _ -> a1

and parse_not (toks : token list) : (token list * expr) =
  match lookahead toks with
  | Tok_Op_LNot ->
      let a1 = parse_U (match_token toks Tok_Op_LNot) in
      let (toks, expr_u) = a1 in
      (toks, LNot(expr_u))
  | _ -> parse_U toks

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
  | Tok_Bool(b) -> (match_token toks token, Bool(b))
  | Tok_LParen ->
    let a1 = parse_A (match_token toks Tok_LParen) in
    let (toks, expr) = a1 in
    let tail = match_token toks Tok_RParen in
    (tail, expr)
  | _ -> raise (ParseError("Unexpected token '" ^ (string_of_token token) ^ "' in parse_N"))
