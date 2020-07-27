open OUnit2
open Calc.Interface

let assert_source ctxt src expected =
    let actual = Calc.Interface.exec src in
    assert_equal expected actual ~ctxt:ctxt ~printer:Calc.Ast.string_of_primitive
;;
