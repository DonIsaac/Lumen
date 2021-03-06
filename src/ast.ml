(** Expression Types *)
type expr =
  (* Values *)
  | Int of int
  | Bool of bool
  | Var of string
  | Null
  (* Logical Operations *)
  | LOr of expr * expr
  | LAnd of expr * expr
  | LNot of expr
  | StructEq of expr * expr
  | RefEq of expr * expr
  | StructNotEq of expr * expr
  | RefNotEq of expr * expr
  (* Arithmatic Operations *)
  | Neg of expr
  | Plus of expr * expr
  | Minus of expr * expr
  | Mult of expr * expr
  | Div of expr * expr
  | Log of expr * expr
  | Exp of expr * expr
  (* State Modification Operations *)
  | Assign of string * expr
  | Decl of string * expr
  (* Control Flow, Process Structure *)
  | Seq of expr * expr
  | Block of expr
  | If of expr * expr * expr
  | Exit of expr

type primitive = Val_Int of int | Val_Bool of bool | Val_Null
type variable = Var of string * primitive ref | Undefined

exception EvalError of string
exception TypeError of string

let string_of_primitive p =
  match p with
  | Val_Int n   -> string_of_int n
  | Val_Bool p  -> string_of_bool p
  | Val_Null    -> "null"
