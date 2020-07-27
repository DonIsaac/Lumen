open Eval
open Parser
open Lexer
open Ast
(** Executes source code using the interpreter
    [str] a string containing the source code to evaluate
 *)
let exec str = str |> lexer |> parser |> eval
