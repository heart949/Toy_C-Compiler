(*
  符号表实现：用于管理变量、函数、参数的作用域信息。
  支持作用域嵌套，适合语义分析阶段使用。
*)

module StringMap = Map.Make(String)

(* ToyC 支持的类型，直接复用 AST 类型 *)
type typ = Ast.return_type

(* 符号表条目，区分变量和函数 *)
type symbol_info =
  | Var of {
      typ: typ;           (* 变量类型 *)
      declared: bool;     (* 是否已声明 *)
      offset: int;        (* 在栈帧中的偏移量，后续可用于代码生成 *)
    }
  | Func of {
      ret_type: typ;                (* 返回类型 *)
      params: (typ * string) list;  (* 参数类型和名称 *)
      declared: bool;               (* 是否已声明 *)
    }

(* 单个作用域，用 StringMap 存储符号名到条目的映射 *)
type scope = symbol_info StringMap.t

(* 符号表整体，使用栈结构，栈顶为当前作用域 *)
type t = scope list

(* 进入新作用域：在栈顶压入一个空的 StringMap *)
let enter_scope (symtab : t) : t = StringMap.empty :: symtab

(* 退出当前作用域：弹出栈顶作用域 *)
let exit_scope (symtab : t) : t =
  match symtab with
  | _ :: rest -> rest
  | [] -> failwith "符号表作用域栈为空，无法退出作用域"

(* 在当前作用域添加符号（声明） *)
let add_symbol (name : string) (info : symbol_info) (symtab : t) : t =
  match symtab with
  | scope :: rest -> (StringMap.add name info scope) :: rest
  | [] -> failwith "符号表作用域栈为空，无法添加符号"

(* 查找符号：从栈顶到栈底依次查找，返回第一个匹配的符号信息 *)
let rec find_symbol (name : string) (symtab : t) : symbol_info option =
  match symtab with
  | [] -> None
  | scope :: rest ->
      (match StringMap.find_opt name scope with
      | Some info -> Some info
      | None -> find_symbol name rest)

(* 判断符号是否在当前作用域已声明（不查父作用域） *)
let is_declared_in_current_scope (name : string) (symtab : t) : bool =
  match symtab with
  | scope :: _ -> StringMap.mem name scope
  | [] -> false 