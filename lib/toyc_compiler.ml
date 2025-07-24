(* 导出主要模块 *)
module Ast = Ast
module Symbol = Symbol
module Riscv = Riscv
module Codegen = Codegen

(* 导出主要分析功能 *)
let analyze_program = Semantic.analyze_program

exception Semantic_error = Semantic.Semantic_error
exception NotAllPathsReturn = Semantic.NotAllPathsReturn

(* 导出代码生成功能 *)
let compile_to_riscv = Codegen.compile_to_riscv
