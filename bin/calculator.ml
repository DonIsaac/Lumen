open Calc.Lexer
open Calc.Parser
open Calc.Eval
open Calc.Ast
open Calc.Interface
open ANSITerminal
open Array

(** Prints a formatted error message to stderr *)
let prerr_endline str = ANSITerminal.prerr_string [Foreground(Red); Bold] str ; prerr_newline ()
let print_endline_colorless = print_endline
(** Prints a formatted message to stdout *)
let print_endline str = ANSITerminal.print_string [Foreground(Green)] str ; print_newline ()

let usage = "TODO add usage and stuff"
let run (line : string) =
  try
    Some (exec line)
  with
    | IllegalExpression(msg) -> prerr_endline ("TokenError: " ^ msg); None
    | ParseError(msg) -> prerr_endline ("ParseError: " ^ msg); None
    | EvalError(msg) -> prerr_endline ("SyntaxError: " ^ msg); None
    | TypeError(msg) -> prerr_endline ("TypeError: " ^ msg); None
    | Division_by_zero -> prerr_endline ("ArithmaticError: Divide by zero"); None
    | Failure(msg) -> prerr_endline ("Error: " ^ msg); None
    | Invalid_argument(msg) -> prerr_endline ("Unexpected Exception: " ^ msg); exit 1
    | e -> prerr_endline ( (Printexc.to_string e) ^ (Printexc.get_backtrace ()) ); None
    (* | _ -> prerr_endline ("An unknown error occurred"); loop () *)
;;
(** REPL loop *)
let rec loop () =
  print_string [Foreground(White); Bold] "> ";
  let input = read_line () in
  let result = run input in
  let () = match result with
    | Some res ->
      let res_str = match res with
        | Val_Int(n)  -> string_of_int n
        | Val_Bool(b) -> string_of_bool b
        | Val_Null    -> "null"
      in
      print_endline @@ "= " ^ res_str;
    | None -> ()
  in
  loop ();
;;

(** Read in the contents of a file as a string*)
let read_from_file (filename : string) : string =
  let read_lines name : string list =
    let input = open_in name in
    let try_read () =
      try Some(input_line input) with End_of_file -> None in
    let rec loop acc = match try_read () with
      | Some s ->
        loop ((s ^ "\n") :: acc)
      | None ->
        close_in input;
        List.rev acc
    in
    loop []
  in
  let prog_lines = read_lines filename in
  List.fold_left (fun a e -> a ^ e) "" prog_lines
;;
(* Entry point *)
(* let _ = Printexc.record_backtrace true; *)

let _ =
  let argv = to_list Sys.argv in
  match argv with
  | [] | _::[] -> loop()
  | _::args ->
    match args with
    | flag::expr::_ when (flag = "-e" || flag = "--eval") ->
      let res = run expr in
      begin
        match res with
          | Some(res) -> print_endline_colorless (string_of_primitive res); exit 0
          | None      -> exit 1
      end
    | filename::_ ->
      begin
        let code = read_from_file filename in
        match run code with
          | Some(_) -> exit 0
          | None    -> exit 1
      end
      (* let result = run code in *)
      (* let _ = string_of_primitive result in *)
      (* TODO do something with result *)
    | _ -> print_endline usage
;;
