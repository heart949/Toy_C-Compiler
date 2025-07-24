type operand =
  | Reg of string (* 临时寄存器 *)
  | Imm of int (* 立即数 *)
  | Var of string (* 变量名 *)

type ir_inst =
  | Binop of string * operand * operand * operand (* t1 = t2 + t3 *)
  | Unop of string * operand * operand (* t1 = -t2 *)
  | Load of operand * operand (* t1 = *t2 *)
  | Store of operand * operand (* *t1 = t2 *)
  | Goto of string
  | IfGoto of operand * string
  | Label of string
  | Call of operand * string * operand list (* t1 = call f(args) *)
  | Ret of operand option
  | Assign of operand * operand (* t1 = t2 *)

type ir_func = { name : string; args : string list; body : ir_inst list }
type ir_program = ir_func list
