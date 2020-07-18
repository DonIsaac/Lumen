(** Expression Types *)
type expr =
  | Int of int
  | Bool of bool
  | LOr of expr * expr
  | LAnd of expr * expr
  | LNot of expr
  | StructEq of expr * expr
  | RefEq of expr * expr
  | StructNotEq of expr * expr
  | RefNotEq of expr * expr
  | Neg of expr
  | Plus of expr * expr
  | Minus of expr * expr
  | Mult of expr * expr
  | Div of expr * expr
  | Decl of string * expr
  | Assign of string * expr
  | Var of string
  | Log of expr * expr
  | Exp of expr * expr
  | Exit of expr


type primitive = Val_Int of int | Val_Bool of bool
type variable = Var of string * primitive ref | Undefined
