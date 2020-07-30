open OUnit2
open Lumen.Interface

let assert_source ctxt src expected =
    let actual = Lumen.Interface.exec src in
    assert_equal expected actual ~ctxt:ctxt ~printer:Lumen.Ast.string_of_primitive
;;
