open OUnit2

let make_func ?(params = []) ?(locals = []) ?(ret_type = Toyc_compiler__Ast.Int) name body
  =
  { Toyc_compiler__Ast.ret_type; name; params; locals; body }
;;

let make_var_decl name value =
  Toyc_compiler__Ast.Declare (name, Toyc_compiler__Ast.Int value)
;;

let make_assign name value = Toyc_compiler__Ast.Assign (name, Toyc_compiler__Ast.Int value)
let make_return value = Toyc_compiler__Ast.Return (Some (Toyc_compiler__Ast.Int value))
let make_void_return () = Toyc_compiler__Ast.Return None

let make_main_func ?(params = []) ?(ret_type = Toyc_compiler__Ast.Int) body =
  make_func "main" ~params ~ret_type body
;;

let print_result name result =
  Printf.printf
    "[%s] %s\n------------------------------------------------------\n%!"
    name
    result
;;

let assert_semantic_ok name prog =
  try
    Toyc_compiler.analyze_program prog;
    print_result name "OK";
    assert_bool (name ^ " should be accepted") true
  with
  | Toyc_compiler.NotAllPathsReturn msg ->
    print_result name ("NotAllPathsReturn: " ^ msg);
    assert_failure (name ^ " should be accepted, but got NotAllPathsReturn: " ^ msg)
  | Toyc_compiler.Semantic_error msg ->
    print_result name ("Semantic error: " ^ msg);
    assert_failure (name ^ " should be accepted, but got error: " ^ msg)
  | e -> raise e
;;

let assert_semantic_error ?(expect_not_all_paths_return = false) name prog expected_msg =
  try
    Toyc_compiler.analyze_program prog;
    print_result name "FAILED (should error)";
    assert_failure (name ^ " should raise error: " ^ expected_msg)
  with
  | Toyc_compiler.NotAllPathsReturn msg when expect_not_all_paths_return ->
    print_result name ("NotAllPathsReturn: " ^ msg);
    assert_equal expected_msg msg ~msg:(name ^ " NotAllPathsReturn message mismatch")
  | Toyc_compiler.Semantic_error msg ->
    print_result name ("Semantic error: " ^ msg);
    assert_equal expected_msg msg ~msg:(name ^ " error message mismatch")
  | Toyc_compiler.NotAllPathsReturn msg ->
    print_result name ("NotAllPathsReturn: " ^ msg);
    assert_failure
      (name ^ " should raise Semantic_error, but got NotAllPathsReturn: " ^ msg)
  | e -> raise e
;;

(* 正确用例 *)
let test_main_ok _ =
  let prog = [ make_main_func (Toyc_compiler__Ast.Block [ make_return 0 ]) ] in
  assert_semantic_ok "main_ok" prog
;;

let test_var_decl_and_assign _ =
  let body =
    Toyc_compiler__Ast.Block [ make_var_decl "a" 10; make_assign "a" 20; make_return 0 ]
  in
  let prog = [ make_main_func body ] in
  assert_semantic_ok "var_decl_and_assign" prog
;;

let test_nested_scope_ok _ =
  let body =
    Toyc_compiler__Ast.Block
      [ make_var_decl "x" 1
      ; Toyc_compiler__Ast.Block
          [ make_var_decl "y" 2; make_assign "x" 3; make_assign "y" 4 ]
      ; make_assign "x" 5
      ; make_return 0
      ]
  in
  let prog = [ make_main_func body ] in
  assert_semantic_ok "nested_scope_ok" prog
;;

let test_if_else_return_ok _ =
  let body =
    Toyc_compiler__Ast.Block
      [ Toyc_compiler__Ast.If
          (Toyc_compiler__Ast.Int 1, make_return 1, Some (make_return 2))
      ]
  in
  let prog = [ make_main_func body ] in
  assert_semantic_ok "if_else_return_ok" prog
;;

let test_while_ok _ =
  let body =
    Toyc_compiler__Ast.Block
      [ Toyc_compiler__Ast.While
          (Toyc_compiler__Ast.Int 1, Toyc_compiler__Ast.Block [ make_return 1 ])
      ]
  in
  let prog = [ make_main_func body ] in
  assert_semantic_ok "while_ok" prog
;;

let test_func_call_ok _ =
  let foo =
    make_func
      "foo"
      ~params:[ Toyc_compiler__Ast.Param "x" ]
      (Toyc_compiler__Ast.Block [ make_return 1 ])
  in
  let main_body =
    Toyc_compiler__Ast.Block
      [ Toyc_compiler__Ast.Expr
          (Toyc_compiler__Ast.Call ("foo", [ Toyc_compiler__Ast.Int 10 ]))
      ; make_return 0
      ]
  in
  let prog = [ foo; make_main_func main_body ] in
  assert_semantic_ok "func_call_ok" prog
;;

let test_void_func_ok _ =
  let foo =
    make_func
      "foo"
      ~ret_type:Toyc_compiler__Ast.Void
      (Toyc_compiler__Ast.Block [ make_void_return () ])
  in
  let main_body =
    Toyc_compiler__Ast.Block
      [ Toyc_compiler__Ast.Expr (Toyc_compiler__Ast.Call ("foo", [])); make_return 0 ]
  in
  let prog = [ foo; make_main_func main_body ] in
  assert_semantic_ok "void_func_ok" prog
;;

let test_var_shadowing_ok _ =
  let body =
    Toyc_compiler__Ast.Block
      [ make_var_decl "a" 1
      ; Toyc_compiler__Ast.Block [ make_var_decl "a" 2; make_assign "a" 3 ]
      ; make_assign "a" 4
      ; make_return 0
      ]
  in
  let prog = [ make_main_func body ] in
  assert_semantic_ok "var_shadowing_ok" prog
;;

let test_complex_expr_ok _ =
  let body =
    Toyc_compiler__Ast.Block
      [ make_var_decl "a" 10
      ; make_var_decl "b" 20
      ; make_var_decl "c" 0
      ; make_assign "c" (10 + (20 * 3) - (4 / 2))
      ; Toyc_compiler__Ast.Expr
          (Toyc_compiler__Ast.BinaryOp
             ( Toyc_compiler__Ast.And
             , Toyc_compiler__Ast.BinaryOp
                 ( Toyc_compiler__Ast.Lt
                 , Toyc_compiler__Ast.Int 1
                 , Toyc_compiler__Ast.Int 2 )
             , Toyc_compiler__Ast.BinaryOp
                 ( Toyc_compiler__Ast.Gt
                 , Toyc_compiler__Ast.Int 3
                 , Toyc_compiler__Ast.Int 0 ) ))
      ; make_return 0
      ]
  in
  let prog = [ make_main_func body ] in
  assert_semantic_ok "complex_expr_ok" prog
;;

let test_multi_func_call_ok _ =
  let foo =
    make_func
      "foo"
      ~params:[ Toyc_compiler__Ast.Param "x" ]
      (Toyc_compiler__Ast.Block [ make_return 1 ])
  in
  let bar = make_func "bar" (Toyc_compiler__Ast.Block [ make_return 2 ]) in
  let main_body =
    Toyc_compiler__Ast.Block
      [ make_var_decl "a" 0
      ; make_assign "a" 1
      ; Toyc_compiler__Ast.Expr
          (Toyc_compiler__Ast.Call ("foo", [ Toyc_compiler__Ast.Int 10 ]))
      ; Toyc_compiler__Ast.Expr (Toyc_compiler__Ast.Call ("bar", []))
      ; make_return 0
      ]
  in
  let prog = [ foo; bar; make_main_func main_body ] in
  assert_semantic_ok "multi_func_call_ok" prog
;;

(* 错误用例 *)
let test_main_missing _ =
  let prog = [ make_func "foo" (Toyc_compiler__Ast.Block [ make_return 1 ]) ] in
  assert_semantic_error "main_missing" prog "必须有一个名为 main、无参数且返回 int 的主函数"
;;

let test_main_with_param _ =
  let prog =
    [ make_func
        "main"
        ~params:[ Toyc_compiler__Ast.Param "a" ]
        (Toyc_compiler__Ast.Block [ make_return 0 ])
    ]
  in
  assert_semantic_error "main_with_param" prog "必须有一个名为 main、无参数且返回 int 的主函数"
;;

let test_main_no_return _ =
  let prog =
    [ make_main_func
        (Toyc_compiler__Ast.Block [ Toyc_compiler__Ast.Expr (Toyc_compiler__Ast.Int 1) ])
    ]
  in
  assert_semantic_error
    ~expect_not_all_paths_return:true
    "main_no_return"
    prog
    "int 类型函数 main 不是所有路径都返回值"
;;

let test_var_undeclared _ =
  let body = Toyc_compiler__Ast.Block [ make_assign "a" 10; make_return 0 ] in
  let prog = [ make_main_func body ] in
  assert_semantic_error "var_undeclared" prog "未声明的变量：a"
;;

let test_var_redeclared _ =
  let body =
    Toyc_compiler__Ast.Block [ make_var_decl "a" 1; make_var_decl "a" 2; make_return 0 ]
  in
  let prog = [ make_main_func body ] in
  assert_semantic_error "var_redeclared" prog "变量重复声明：a"
;;

let test_func_call_param_count _ =
  let foo =
    make_func
      "foo"
      ~params:[ Toyc_compiler__Ast.Param "x" ]
      ~ret_type:Toyc_compiler__Ast.Void
      (Toyc_compiler__Ast.Block [ make_void_return () ])
  in
  let main_body =
    Toyc_compiler__Ast.Block
      [ Toyc_compiler__Ast.Expr
          (Toyc_compiler__Ast.Call
             ("foo", [ Toyc_compiler__Ast.Int 1; Toyc_compiler__Ast.Int 2 ]))
      ; make_return 0
      ]
  in
  let prog = [ foo; make_main_func main_body ] in
  assert_semantic_error "func_call_param_count" prog "函数参数数量不匹配：foo"
;;

let test_func_call_param_type _ =
  let foo =
    make_func
      "foo"
      ~params:[ Toyc_compiler__Ast.Param "x" ]
      ~ret_type:Toyc_compiler__Ast.Void
      (Toyc_compiler__Ast.Block [ make_void_return () ])
  in
  let bar =
    make_func
      "bar"
      ~ret_type:Toyc_compiler__Ast.Void
      (Toyc_compiler__Ast.Block [ make_void_return () ])
  in
  let main_body =
    Toyc_compiler__Ast.Block
      [ Toyc_compiler__Ast.Expr
          (Toyc_compiler__Ast.Call ("foo", [ Toyc_compiler__Ast.Call ("bar", []) ]))
      ; make_return 0
      ]
  in
  let prog = [ foo; bar; make_main_func main_body ] in
  assert_semantic_error "func_call_param_type" prog "函数参数类型不匹配：foo"
;;

let test_void_func_param_count _ =
  let foo =
    make_func
      "foo"
      ~params:[]
      ~ret_type:Toyc_compiler__Ast.Void
      (Toyc_compiler__Ast.Block [ make_void_return () ])
  in
  let main_body =
    Toyc_compiler__Ast.Block
      [ Toyc_compiler__Ast.Expr
          (Toyc_compiler__Ast.Call ("foo", [ Toyc_compiler__Ast.Int 1 ]))
      ; make_return 0
      ]
  in
  let prog = [ foo; make_main_func main_body ] in
  assert_semantic_error "void_func_param_count" prog "函数参数数量不匹配：foo"
;;

let test_void_func_param_type _ =
  let foo =
    make_func
      "foo"
      ~params:[ Toyc_compiler__Ast.Param "x" ]
      ~ret_type:Toyc_compiler__Ast.Void
      (Toyc_compiler__Ast.Block [ make_void_return () ])
  in
  let bar =
    make_func
      "bar"
      ~ret_type:Toyc_compiler__Ast.Void
      (Toyc_compiler__Ast.Block [ make_void_return () ])
  in
  let main_body =
    Toyc_compiler__Ast.Block
      [ Toyc_compiler__Ast.Expr
          (Toyc_compiler__Ast.Call ("foo", [ Toyc_compiler__Ast.Call ("bar", []) ]))
      ; make_return 0
      ]
  in
  let prog = [ foo; bar; make_main_func main_body ] in
  assert_semantic_error "void_func_param_type" prog "函数参数类型不匹配：foo"
;;

let test_call_undeclared_func _ =
  let main_body =
    Toyc_compiler__Ast.Block
      [ Toyc_compiler__Ast.Expr
          (Toyc_compiler__Ast.Call ("bar", [ Toyc_compiler__Ast.Int 1 ]))
      ; make_return 0
      ]
  in
  let prog = [ make_main_func main_body ] in
  assert_semantic_error "call_undeclared_func" prog "未声明的函数：bar"
;;

let test_call_var_as_func _ =
  let body =
    Toyc_compiler__Ast.Block
      [ make_var_decl "a" 1
      ; Toyc_compiler__Ast.Expr (Toyc_compiler__Ast.Call ("a", []))
      ; make_return 0
      ]
  in
  let prog = [ make_main_func body ] in
  assert_semantic_error "call_var_as_func" prog "不能将变量作为函数调用：a"
;;

let test_assign_func_name _ =
  let foo =
    make_func
      "foo"
      ~ret_type:Toyc_compiler__Ast.Void
      (Toyc_compiler__Ast.Block [ make_void_return () ])
  in
  let body =
    Toyc_compiler__Ast.Block
      [ Toyc_compiler__Ast.Assign ("foo", Toyc_compiler__Ast.Int 1); make_return 0 ]
  in
  let prog = [ foo; make_main_func body ] in
  assert_semantic_error "assign_func_name" prog "不能给函数名赋值：foo"
;;

let test_break_outside_loop _ =
  let body = Toyc_compiler__Ast.Block [ Toyc_compiler__Ast.Break; make_return 0 ] in
  let prog = [ make_main_func body ] in
  assert_semantic_error "break_outside_loop" prog "break 语句只能出现在循环内部"
;;

let test_var_shadowing _ =
  let body =
    Toyc_compiler__Ast.Block
      [ make_var_decl "a" 1
      ; Toyc_compiler__Ast.Block [ make_var_decl "a" 2; make_assign "a" 3 ]
      ; make_assign "a" 4
      ; make_return 0
      ]
  in
  let prog = [ make_main_func body ] in
  assert_semantic_ok "var_shadowing" prog
;;

let test_void_func_return_value _ =
  let foo =
    make_func
      "foo"
      ~ret_type:Toyc_compiler__Ast.Void
      (Toyc_compiler__Ast.Block [ make_return 1 ])
  in
  let main_body = Toyc_compiler__Ast.Block [ make_return 0 ] in
  let prog = [ foo; make_main_func main_body ] in
  assert_semantic_error "void_func_return_value" prog "void 函数不能返回值"
;;

let test_if_condition_type _ =
  let body =
    Toyc_compiler__Ast.Block
      [ Toyc_compiler__Ast.If
          (Toyc_compiler__Ast.Call ("foo", []), make_return 1, Some (make_return 0))
      ]
  in
  let foo =
    make_func
      "foo"
      ~ret_type:Toyc_compiler__Ast.Void
      (Toyc_compiler__Ast.Block [ make_void_return () ])
  in
  let prog = [ foo; make_main_func body ] in
  assert_semantic_error "if_condition_type" prog "if 条件类型必须为 int"
;;

let test_break_continue_in_loop _ =
  let body =
    Toyc_compiler__Ast.Block
      [ Toyc_compiler__Ast.While
          ( Toyc_compiler__Ast.Int 1
          , Toyc_compiler__Ast.Block
              [ Toyc_compiler__Ast.Break; Toyc_compiler__Ast.Continue; make_return 0 ] )
      ; make_return 0
      ]
  in
  let prog = [ make_main_func body ] in
  assert_semantic_ok "break_continue_in_loop" prog
;;

let test_void_func_no_return_ok _ =
  let foo =
    make_func "foo" ~ret_type:Toyc_compiler__Ast.Void (Toyc_compiler__Ast.Block [])
  in
  let main_body =
    Toyc_compiler__Ast.Block
      [ Toyc_compiler__Ast.Expr (Toyc_compiler__Ast.Call ("foo", [])); make_return 0 ]
  in
  let prog = [ foo; make_main_func main_body ] in
  assert_semantic_ok "void_func_no_return_ok" prog
;;

let test_func_name_duplicate _ =
  let foo1 = make_func "foo" (Toyc_compiler__Ast.Block [ make_void_return () ]) in
  let foo2 = make_func "foo" (Toyc_compiler__Ast.Block [ make_void_return () ]) in
  let prog =
    [ foo1; foo2; make_main_func (Toyc_compiler__Ast.Block [ make_return 0 ]) ]
  in
  assert_semantic_error "func_name_duplicate" prog "重复定义的函数名：foo"
;;

let test_int_func_no_return _ =
  let foo =
    make_func
      "foo"
      ~ret_type:Toyc_compiler__Ast.Int
      (Toyc_compiler__Ast.Block [ Toyc_compiler__Ast.Expr (Toyc_compiler__Ast.Int 1) ])
  in
  let main_body = Toyc_compiler__Ast.Block [ make_return 0 ] in
  let prog = [ foo; make_main_func main_body ] in
  assert_semantic_error
    ~expect_not_all_paths_return:true
    "int_func_no_return"
    prog
    "int 类型函数 foo 不是所有路径都返回值"
;;

let test_div_by_zero _ =
  let body =
    Toyc_compiler__Ast.Block
      [ make_var_decl "a" 10
      ; make_assign "a" 0
      ; Toyc_compiler__Ast.Expr
          (Toyc_compiler__Ast.BinaryOp
             (Toyc_compiler__Ast.Div, Toyc_compiler__Ast.Int 1, Toyc_compiler__Ast.Int 0))
      ; make_return 0
      ]
  in
  let prog = [ make_main_func body ] in
  assert_semantic_error "div_by_zero" prog "除数不能为零"
;;

let suite =
  "ToyC Semantic Analyzer"
  >::: [ "main_ok" >:: test_main_ok
       ; "var_decl_and_assign" >:: test_var_decl_and_assign
       ; "nested_scope_ok" >:: test_nested_scope_ok
       ; "if_else_return_ok" >:: test_if_else_return_ok
       ; "while_ok" >:: test_while_ok
       ; "func_call_ok" >:: test_func_call_ok
       ; "void_func_ok" >:: test_void_func_ok
       ; "var_shadowing_ok" >:: test_var_shadowing_ok
       ; "complex_expr_ok" >:: test_complex_expr_ok
       ; "multi_func_call_ok" >:: test_multi_func_call_ok
       ; "main_missing" >:: test_main_missing
       ; "main_with_param" >:: test_main_with_param
       ; "main_no_return" >:: test_main_no_return
       ; "var_undeclared" >:: test_var_undeclared
       ; "var_redeclared" >:: test_var_redeclared
       ; "func_call_param_count" >:: test_func_call_param_count
       ; "func_call_param_type" >:: test_func_call_param_type
       ; "void_func_param_count" >:: test_void_func_param_count
       ; "void_func_param_type" >:: test_void_func_param_type
       ; "call_undeclared_func" >:: test_call_undeclared_func
       ; "call_var_as_func" >:: test_call_var_as_func
       ; "assign_func_name" >:: test_assign_func_name
       ; "break_outside_loop" >:: test_break_outside_loop
       ; "var_shadowing" >:: test_var_shadowing
       ; "void_func_return_value" >:: test_void_func_return_value
       ; "if_condition_type" >:: test_if_condition_type
       ; "break_continue_in_loop" >:: test_break_continue_in_loop
       ; "void_func_no_return_ok" >:: test_void_func_no_return_ok
       ; "func_name_duplicate" >:: test_func_name_duplicate
       ; "int_func_no_return" >:: test_int_func_no_return
       ; "div_by_zero" >:: test_div_by_zero
       ]
;;

let () = run_test_tt_main suite
