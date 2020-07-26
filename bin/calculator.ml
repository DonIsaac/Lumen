open Calc.Lexer
open Calc.Parser
open Calc.Interpreter
open Calc.Ast
open ANSITerminal

let exec str = eval ( parser ( lexer str ))

(** Prints a formatted error message to stderr *)
let prerr_endline str = ANSITerminal.prerr_string [Foreground(Red); Bold] str ; prerr_newline ()
;;
(** Prints a formatted message to stdout *)
let print_endline str = ANSITerminal.print_string [Foreground(Green)] str ; print_newline ()
;;
(** REPL loop *)
let rec loop () =
  print_string [Foreground(White); Bold] "> ";
  let input = read_line () in
  try
    begin
      let res = match exec input with
        | Val_Int(n) -> string_of_int n
        | Val_Bool(b) -> string_of_bool b
      in
      print_endline @@ "= " ^ res;
      loop ();
    end
  with
    | IllegalExpression(msg) -> prerr_endline ("TokenError: " ^ msg); loop ()
    | ParseError(msg) -> prerr_endline ("ParseError: " ^ msg); loop ()
    | EvalError(msg) -> prerr_endline ("SyntaxError: " ^ msg); loop ()
    | TypeError(msg) -> prerr_endline ("TypeError: " ^ msg); loop ()
    | Division_by_zero -> prerr_endline ("ArithmaticError: Divide by zero"); loop ()
    | Failure(msg) -> prerr_endline ("Error: " ^ msg); loop()
    | Invalid_argument(msg) -> prerr_endline ("Unexpected Exception: " ^ msg); exit 1
    | _ -> prerr_endline ("An unknown error occurred"); loop ()
;;

(* Entry point *)
loop ();;
(* exec "true and false" *)
