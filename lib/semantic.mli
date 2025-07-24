(*
  语义分析接口：声明主异常和主分析函数。
  供主程序调用。
*)

exception Semantic_error of string
exception NotAllPathsReturn of string

val analyze_program : Ast.comp_unit -> unit 