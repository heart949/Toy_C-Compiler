(* 导出主要模块 *)
module Ast = Ast
module Symbol = Symbol
module Riscv = Riscv
module Codegen = Codegen

(* 导出主要分析功能 *)
val analyze_program : Ast.comp_unit -> unit

exception Semantic_error of string
exception NotAllPathsReturn of string

val compile_to_riscv : Symbol.t -> Ast.comp_unit -> string -> unit
