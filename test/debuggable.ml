open Calc.Eval
open Calc.Interface
open Calc.Lexer
open Calc.Parser
open Calc.Ast
open Util
;;
let input = {|
let x = 5;
let y = true;
let z = if x != y then 9 else 8

|}
;;

let run () =
    let expected = Val_Int 9 in
    let actual = exec input in
    (
        if expected = actual then
            ()
        else
            let _ = raise @@ EvalError ("Expected" ^ (string_of_primitive expected) ^ ", got " ^ (string_of_primitive actual)) in
            ()
    )
;;

run ()
