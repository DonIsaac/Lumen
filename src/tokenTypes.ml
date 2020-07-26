(* Token Type *)
type token =
  | Tok_Int of int
  | Tok_Bool of bool
  | Tok_ID of string
  | Tok_Null
  | Tok_Term
  (* Operators *)
  | Tok_Assign
  | Tok_Op_LAnd           (* Logical AND              *)
  | Tok_Op_LOr            (* Logical OR               *)
  | Tok_Op_LNot
  | Tok_Op_Eq_Struct      (* Structural Equality      *)
  | Tok_Op_Eq_Ref         (* Referential Equality     *)
  | Tok_Op_NEq_Struct     (* Structural Inequality    *)
  | Tok_Op_NEq_Ref        (* Referential Inequality   *)
  | Tok_Mult
  | Tok_Div
  | Tok_Plus
  | Tok_Minus
  | Tok_LParen
  | Tok_RParen
  | Tok_Exp
  (* Keywords *)
  | Tok_Let
  | Tok_Log
  | Tok_Base
  | Tok_Of
  | Tok_EOF
  | Tok_Exit
