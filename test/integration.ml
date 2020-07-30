open OUnit2
open Lumen.TokenTypes
open Lumen.Ast
open Lumen.Lexer
open Lumen.Parser
open Lumen.Eval
open Util

let test_simple_add (ctx: test_ctxt) =
  assert_source ctx "5 + 5" (Val_Int 10)
;;
let test_simple_subtract (ctx: test_ctxt) =
  assert_source ctx "10 - 5" (Val_Int 5)
;;
let test_simple_div (ctx: test_ctxt) =
  assert_source ctx "10 / 5" (Val_Int 2)
;;
let test_simple_mult (ctx: test_ctxt) =
  assert_source ctx "2 * 5" (Val_Int 10)
;;
let test_simple_log_and_expr (ctx: test_ctxt) =
  assert_source ctx "log base 2 of 8" (Val_Int 3);
  assert_source ctx "2 ^ 8" (Val_Int 256)
;;
let test_simple_logical_expr (ctx: test_ctxt) =
  assert_source ctx "true" (Val_Bool true);
  assert_source ctx "false" (Val_Bool false);
  (* And *)
  assert_source ctx "true and true" (Val_Bool true);
  assert_source ctx "true and false" (Val_Bool false);
  assert_source ctx "false and true" (Val_Bool false);
  assert_source ctx "false and false" (Val_Bool false);
  (* Or *)
  assert_source ctx "true or true" (Val_Bool true);
  assert_source ctx "true or false" (Val_Bool true);
  assert_source ctx "false or true" (Val_Bool true);
  assert_source ctx "false or false" (Val_Bool false);

  (* Not *)
  assert_source ctx "not true" (Val_Bool false);
  assert_source ctx "not false" (Val_Bool true);

  (* Complecated stuff *)
  assert_source ctx "true and not false" (Val_Bool true);
  assert_source ctx "true and false or true and true" (Val_Bool true);
  assert_source ctx "true and not false or not true" (Val_Bool true);
  assert_source ctx "true or false and not false" (Val_Bool true)
;;
let test_simple_vars (ctx: test_ctxt) =
  assert_source ctx "let a" Val_Null;
  assert_source ctx "let b = ()" Val_Null;
  assert_source ctx "let c = 5" (Val_Int 5);
  assert_source ctx "let d = 5; let e = true" (Val_Bool true);
  assert_source ctx "let f = 5; let g = 2; let h = 5 + f + g" (Val_Int 12);
  assert_source ctx "let i = 5; let k = 10; i + k" (Val_Int 15)
;;
let test_simple_if (ctx: test_ctxt) =
  assert_source ctx "if true then 5" (Val_Int 5);
  assert_source ctx "if false then 2" Val_Null;
  assert_source ctx "if true then 5 else 2" (Val_Int 5);
  assert_source ctx "if false then 5 else 2" (Val_Int 2);
  assert_source ctx "if false then 4 else if true then 6 else 0" (Val_Int 6)
;;
let test_advanced_if (ctx : test_ctxt) =
  assert_source ctx "let x = 5; if x != 10 then do let y = 9; x + 9; end" (Val_Int 14);
  assert_source ctx "if false then do 9 end else 7" (Val_Int 7);
  assert_source ctx "if false then do true end else do not true end" (Val_Bool false)
;;

let test_program1 (ctx: test_ctxt) =
    assert_source ctx {|let x = 5;
let z = if true and not false then
do
    let y = 8;
    x + y;
end
    else
do
    let y = 9;
    y - x;
end
z = z + 3;
log base 2 of z|} (Val_Int 4)
let suite =
  "suite" >::: [
    "simple" >::: [
      "add" >:: test_simple_add;
      "subtract" >:: test_simple_subtract;
      "divide" >:: test_simple_div;
      "multiply" >:: test_simple_mult;
      "boolean expressions" >:: test_simple_logical_expr;
      "logarithms and exponents" >:: test_simple_log_and_expr;
      "variable declaration and assignment" >:: test_simple_vars;
      "if statements" >:: test_simple_if;
    ];

    "advanced" >::: [
        "if statements" >:: test_advanced_if;
    ];

    "programs" >::: [
        "program 1" >:: test_program1
    ]
  ]
;;
let _ = run_test_tt_main suite
