(* 标识符，就是字符串 *)
type ident = string

(* 二元运算符 *)
type binop =
  | Add (* + *)
  | Sub (* - *)
  | Mul (* * *)
  | Div (* / *)
  | Mod (* % *)
  | Eq (* == *)
  | Neq (* != *)
  | Lt (* < *)
  | Leq (* <= *)
  | Gt (* > *)
  | Geq (* >= *)
  | And (* && *)
  | Or (* || *)

(* 一元运算符 *)
type unop =
  | Neg (* - *)
  | Not (* ! *)

(* 表达式 *)
type expr =
  | Int of int (* 整数常量, e.g., 10 *)
  | Var of ident (* 变量引用, e.g., x *)
  | UnaryOp of unop * expr (* 一元运算, e.g., !a *)
  | BinaryOp of binop * expr * expr (* 二元运算, e.g., a + b *)
  | Call of ident * expr list (* 函数调用, e.g., foo(a, b) *)

(* 语句 *)
type stmt =
  | Empty (* 空语句, e.g., ; *)
  | Expr of expr (* 表达式语句, e.g., foo(); *)
  | Block of stmt list (* 语句块, e.g., { ... } *)
  | Return of expr option (* 返回语句, e.g., return a; or return; *)
  | If of expr * stmt * stmt option (* if-else 语句, e.g., if (c) s1 else s2 *)
  | While of expr * stmt (* while 循环, e.g., while (c) s *)
  | Break (* break; *)
  | Continue (* continue; *)
  | Declare of ident * expr (* 变量声明与初始化, e.g., int a = 10; *)
  | Assign of ident * expr (* 变量赋值, e.g., a = 20; *)

(* 函数返回类型 *)
type return_type =
  | Void
  | Int

(* 函数形参 *)
type param = Param of ident (* int a *)

(* 函数定义 *)
type func_def =
  { ret_type : return_type
  ; name : ident
  ; params : param list
  ; locals : (ident * expr) list (* New field for local declarations *)
  ; body : stmt (* 函数体总是一个 Block *)
  }

(* 编译单元，即整个程序 *)
type comp_unit = func_def list
