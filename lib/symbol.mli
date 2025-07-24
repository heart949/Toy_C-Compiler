(*
  符号表接口：声明符号表相关类型和操作函数。
  供语义分析模块调用。
*)

module StringMap : Map.S with type key = string

type typ = Ast.return_type

type symbol_info =
  | Var of {
      typ: typ;
      declared: bool;
      offset: int;
    }
  | Func of {
      ret_type: typ;
      params: (typ * string) list;
      declared: bool;
    }

type scope

(* 符号表整体类型，t = scope list *)
type t = scope list

val enter_scope : t -> t
val exit_scope : t -> t
val add_symbol : string -> symbol_info -> t -> t
val find_symbol : string -> t -> symbol_info option
val is_declared_in_current_scope : string -> t -> bool 