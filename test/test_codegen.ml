open Toyc_compiler

let test_main_function =
  Ast.
    { ret_type = Int
    ; name = "main"
    ; params = []
    ; locals = [] (* Add this line *)
    ; body =
        Block
          [ Declare ("x", Int 5)
          ; Declare ("y", Int 10)
          ; Declare ("z", BinaryOp (Add, Var "x", Var "y"))
          ; Return (Some (Var "z"))
          ]
    }
;;

let test_program = [ test_main_function ]

let () =
  Printf.printf "测试ToyC代码生成器\n";
  (* 创建空的符号表 *)
  let symtab = Symbol.enter_scope [] in
  (* 生成汇编代码 *)
  let asm_items = Codegen.gen_program symtab test_program in
  (* 输出汇编代码到控制台 *)
  Printf.printf "\n生成的RISC-V汇编代码：\n";
  Printf.printf "%s\n" (Riscv.emit_asm_to_string asm_items);
  (* 输出到文件 *)
  Riscv.emit_asm_to_file "test_output.s" asm_items;
  Printf.printf "\n汇编代码已输出到 test_output.s\n"
;;
