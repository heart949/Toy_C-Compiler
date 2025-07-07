(* 基本类型定义 *)
type typ =
  | TInt
  (* 整数类型 *)
  | TVoid (* 空类型 *)

type bin_op =
  | Add
  | Sub
  | Mul
  | Div
  | Mod
  | Eq
  | Neq
  | Less
  | Leq
  | Greater
  | Geq
  | Land
  | Lor

type un_op = Not | Plus | Minus

type expr =
  | Binop of bin_op * expr * expr
  | Unop of un_op * expr
  | ID of string
  | Number of int
  | Call of string * expr list

(* 语句 *)
type stmt =
  | Block of stmt list (* 语句块 {...} *)
  | Empty (* 空语句 ; *)
  | ExprStmt of expr (* 表达式语句 *)
  | Decl of string * expr option (* 变量声明（带or不带初始化） *)
  | Assign of string * expr (* 赋值语句 *)
  | If of expr * stmt * stmt option (* if语句（else可选） *)
  | While of expr * stmt (* while循环 *)
  | Break (* break语句 *)
  | Continue (* continue语句 *)
  | Return of expr option (* return语句（返回值可选） *)

(* 函数参数 *)
type param = string (* 参数名（类型固定为int） *)

(* 函数定义 *)
type func_def = {
  ret_type : typ; (* 返回类型 *)
  func_name : string; (* 函数名 *)
  params : param list (* 参数列表 *);
  body : stmt list (* 函数体（语句列表） *);
}

(* 编译单元（整个程序） *)
type comp_unit = func_def list (* 函数定义列表 *)
