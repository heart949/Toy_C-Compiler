(*
   ToyC 编译器代码生成模块接口
*)
open Riscv

(* 代码生成上下文 *)
type codegen_context

(* 创建新的代码生成上下文 *)
val create_context : Symbol.t -> codegen_context

(* 生成表达式代码，返回结果寄存器和指令列表 *)
val gen_expr : codegen_context -> Ast.expr -> reg * instruction list

(* 生成语句代码 *)
val gen_stmt : codegen_context -> int -> Ast.stmt -> asm_item list

(* 生成函数代码 *)
val gen_function : Symbol.t -> Ast.func_def -> asm_item list

(* 生成程序代码 *)
val gen_program : Symbol.t -> Ast.comp_unit -> asm_item list

(* 主入口函数：编译程序并输出汇编文件 *)
val compile_to_riscv : Symbol.t -> Ast.comp_unit -> string -> unit
