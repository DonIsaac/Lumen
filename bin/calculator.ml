open Calc.Lexer
open Calc.Parser
open Calc.Interpreter

let exec str = eval ( parser ( lexer str ))

let rec loop () =
    print_string "> ";
    let input = read_line () in
    let res = exec input in
    print_endline @@ string_of_int res;
    loop ();


loop ();;
