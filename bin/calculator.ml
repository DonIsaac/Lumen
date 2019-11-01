open Calc.Lexer
open Calc.Parser
open Calc.Interpreter
open ANSITerminal

let exec str = eval ( parser ( lexer str ))

let prerr_endline str = ANSITerminal.prerr_string [Foreground(Red); Bold] str ; prerr_newline ()
;;
let print_endline str = ANSITerminal.print_string [Foreground(Green)] str ; print_newline ()
;;
let rec loop () =
  print_string [Foreground(White); Bold] "> ";
  let input = read_line () in
  try
    begin
      let res = exec input in
      print_endline @@ "= " ^ (string_of_int res);
      loop ();
    end
  with
    | IllegalExpression(msg) -> prerr_endline ("Token Error: " ^ msg); loop ()
    | ParseError(msg) -> prerr_endline ("Error: " ^ msg); loop ()
    | EvalError(msg) -> prerr_endline ("SyntaxError: " ^ msg); loop ()
    | Division_by_zero -> prerr_endline ("ArithmaticError: Divide by zero"); loop ()
;;


loop ();;
