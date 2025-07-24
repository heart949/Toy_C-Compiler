(*
   RISC-V 32位汇编指令定义和输出模块接口
*)

(* RISC-V 32位寄存器 *)
type reg =
  | Zero
  | Ra
  | Sp
  | Gp
  | Tp
  | T0
  | T1
  | T2
  | Fp (* s0/x8 is used as Frame Pointer *)
  | S1
  | A0
  | A1
  | A2
  | A3
  | A4
  | A5
  | A6
  | A7
  | S2
  | S3
  | S4
  | S5
  | S6
  | S7
  | S8
  | S9
  | S10
  | S11
  | T3
  | T4
  | T5
  | T6

(* RISC-V 指令类型 *)
type instruction =
  (* 算术指令 *)
  | Add of reg * reg * reg
  | Addi of reg * reg * int
  | Sub of reg * reg * reg
  | Mul of reg * reg * reg
  | Div of reg * reg * reg
  | Rem of reg * reg * reg
  (* 逻辑指令 *)
  | And of reg * reg * reg
  | Or of reg * reg * reg
  | Xor of reg * reg * reg
  | Xori of reg * reg * int
  (* 比较指令 *)
  | Slt of reg * reg * reg
  | Slti of reg * reg * int
  | Sltu of reg * reg * reg
  | Sltiu of reg * reg * int
  (* 加载/存储指令 *)
  | Lw of reg * int * reg
  | Sw of reg * int * reg
  (* 分支指令 *)
  | Beq of reg * reg * string
  | Bne of reg * reg * string
  | Blt of reg * reg * string
  | Bge of reg * reg * string
  | Ble of reg * reg * string
  | Bgt of reg * reg * string
  (* 跳转指令 *)
  | J of string
  | Jal of reg * string
  | Jalr of reg * reg * int
  | Ret
  (* 立即数加载 *)
  | Li of reg * int
  | Lui of reg * int
  (* 移动指令 *)
  | Mv of reg * reg
  (* 其他 *)
  | Nop

(* 标签 *)
type label = string

(* 汇编代码项 *)
type asm_item =
  | Label of label
  | Instruction of instruction
  | Comment of string
  | Directive of string

(* 函数声明 *)
val reg_to_string : reg -> string
val instruction_to_string : instruction -> string
val asm_item_to_string : asm_item -> string
val emit_asm_to_file : string -> asm_item list -> unit
val emit_asm_to_string : asm_item list -> string
