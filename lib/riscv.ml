(*
   RISC-V 32位汇编指令定义和输出模块
  定义 RISC-V 指令类型，并提供将指令转换为汇编字符串的功能
*)

(* RISC-V 32位寄存器 *)
type reg =
  | Zero (* x0: 常量0 *)
  | Ra (* x1: 返回地址 *)
  | Sp (* x2: 栈指针 *)
  | Gp (* x3: 全局指针 *)
  | Tp (* x4: 线程指针 *)
  | T0 (* x5: 临时寄存器 *)
  | T1 (* x6: 临时寄存器 *)
  | T2 (* x7: 临时寄存器 *)
  | Fp (* x8: 帧指针 *)
  | S1 (* x9: 保存寄存器 *)
  | A0 (* x10: 函数参数/返回值 *)
  | A1 (* x11: 函数参数 *)
  | A2 (* x12: 函数参数 *)
  | A3 (* x13: 函数参数 *)
  | A4 (* x14: 函数参数 *)
  | A5 (* x15: 函数参数 *)
  | A6 (* x16: 函数参数 *)
  | A7 (* x17: 函数参数 *)
  | S2 (* x18: 保存寄存器 *)
  | S3 (* x19: 保存寄存器 *)
  | S4 (* x20: 保存寄存器 *)
  | S5 (* x21: 保存寄存器 *)
  | S6 (* x22: 保存寄存器 *)
  | S7 (* x23: 保存寄存器 *)
  | S8 (* x24: 保存寄存器 *)
  | S9 (* x25: 保存寄存器 *)
  | S10 (* x26: 保存寄存器 *)
  | S11 (* x27: 保存寄存器 *)
  | T3 (* x28: 临时寄存器 *)
  | T4 (* x29: 临时寄存器 *)
  | T5 (* x30: 临时寄存器 *)
  | T6 (* x31: 临时寄存器 *)

(* RISC-V 指令类型 *)
type instruction =
  (* 算术指令 *)
  | Add of reg * reg * reg (* add rd, rs1, rs2 *)
  | Addi of reg * reg * int (* addi rd, rs1, imm *)
  | Sub of reg * reg * reg (* sub rd, rs1, rs2 *)
  | Mul of reg * reg * reg (* mul rd, rs1, rs2 *)
  | Div of reg * reg * reg (* div rd, rs1, rs2 *)
  | Rem of reg * reg * reg (* rem rd, rs1, rs2 *)
  (* 逻辑指令 *)
  | And of reg * reg * reg (* and rd, rs1, rs2 *)
  | Or of reg * reg * reg (* or rd, rs1, rs2 *)
  | Xor of reg * reg * reg (* xor rd, rs1, rs2 *)
  | Xori of reg * reg * int (* xori rd, rs1, imm *)
  (* 比较指令 *)
  | Slt of reg * reg * reg (* slt rd, rs1, rs2 *)
  | Slti of reg * reg * int (* slti rd, rs1, imm *)
  | Sltu of reg * reg * reg (* sltu rd, rs1, rs2 *)
  | Sltiu of reg * reg * int (* sltiu rd, rs1, imm *)
  (* 加载/存储指令 *)
  | Lw of reg * int * reg (* lw rd, offset(rs1) *)
  | Sw of reg * int * reg (* sw rs2, offset(rs1) *)
  (* 分支指令 *)
  | Beq of reg * reg * string (* beq rs1, rs2, label *)
  | Bne of reg * reg * string (* bne rs1, rs2, label *)
  | Blt of reg * reg * string (* blt rs1, rs2, label *)
  | Bge of reg * reg * string (* bge rs1, rs2, label *)
  | Ble of reg * reg * string (* ble rs1, rs2, label - 伪指令 *)
  | Bgt of reg * reg * string (* bgt rs1, rs2, label - 伪指令 *)
  (* 跳转指令 *)
  | J of string (* j label *)
  | Jal of reg * string (* jal rd, label *)
  | Jalr of reg * reg * int (* jalr rd, rs1, offset *)
  | Ret (* ret - 伪指令 *)
  (* 立即数加载 *)
  | Li of reg * int (* li rd, imm - 伪指令 *)
  | Lui of reg * int (* lui rd, imm *)
  (* 移动指令 *)
  | Mv of reg * reg (* mv rd, rs - 伪指令 *)
  (* 其他 *)
  | Nop (* nop *)

(* 标签 *)
type label = string

(* 汇编代码项 *)
type asm_item =
  | Label of label
  | Instruction of instruction
  | Comment of string
  | Directive of string

(* 将寄存器转换为字符串 *)
let reg_to_string = function
  | Zero -> "zero"
  | Ra -> "ra"
  | Sp -> "sp"
  | Gp -> "gp"
  | Tp -> "tp"
  | T0 -> "t0"
  | T1 -> "t1"
  | T2 -> "t2"
  | Fp -> "fp"
  | S1 -> "s1"
  | A0 -> "a0"
  | A1 -> "a1"
  | A2 -> "a2"
  | A3 -> "a3"
  | A4 -> "a4"
  | A5 -> "a5"
  | A6 -> "a6"
  | A7 -> "a7"
  | S2 -> "s2"
  | S3 -> "s3"
  | S4 -> "s4"
  | S5 -> "s5"
  | S6 -> "s6"
  | S7 -> "s7"
  | S8 -> "s8"
  | S9 -> "s9"
  | S10 -> "s10"
  | S11 -> "s11"
  | T3 -> "t3"
  | T4 -> "t4"
  | T5 -> "t5"
  | T6 -> "t6"
;;

(* 将指令转换为汇编字符串 *)
let instruction_to_string = function
  | Add (rd, rs1, rs2) ->
    Printf.sprintf
      "add %s, %s, %s"
      (reg_to_string rd)
      (reg_to_string rs1)
      (reg_to_string rs2)
  | Addi (rd, rs1, imm) ->
    Printf.sprintf "addi %s, %s, %d" (reg_to_string rd) (reg_to_string rs1) imm
  | Sub (rd, rs1, rs2) ->
    Printf.sprintf
      "sub %s, %s, %s"
      (reg_to_string rd)
      (reg_to_string rs1)
      (reg_to_string rs2)
  | Mul (rd, rs1, rs2) ->
    Printf.sprintf
      "mul %s, %s, %s"
      (reg_to_string rd)
      (reg_to_string rs1)
      (reg_to_string rs2)
  | Div (rd, rs1, rs2) ->
    Printf.sprintf
      "div %s, %s, %s"
      (reg_to_string rd)
      (reg_to_string rs1)
      (reg_to_string rs2)
  | Rem (rd, rs1, rs2) ->
    Printf.sprintf
      "rem %s, %s, %s"
      (reg_to_string rd)
      (reg_to_string rs1)
      (reg_to_string rs2)
  | And (rd, rs1, rs2) ->
    Printf.sprintf
      "and %s, %s, %s"
      (reg_to_string rd)
      (reg_to_string rs1)
      (reg_to_string rs2)
  | Or (rd, rs1, rs2) ->
    Printf.sprintf
      "or %s, %s, %s"
      (reg_to_string rd)
      (reg_to_string rs1)
      (reg_to_string rs2)
  | Xor (rd, rs1, rs2) ->
    Printf.sprintf
      "xor %s, %s, %s"
      (reg_to_string rd)
      (reg_to_string rs1)
      (reg_to_string rs2)
  | Xori (rd, rs1, imm) ->
    Printf.sprintf "xori %s, %s, %d" (reg_to_string rd) (reg_to_string rs1) imm
  | Slt (rd, rs1, rs2) ->
    Printf.sprintf
      "slt %s, %s, %s"
      (reg_to_string rd)
      (reg_to_string rs1)
      (reg_to_string rs2)
  | Slti (rd, rs1, imm) ->
    Printf.sprintf "slti %s, %s, %d" (reg_to_string rd) (reg_to_string rs1) imm
  | Sltu (rd, rs1, rs2) ->
    Printf.sprintf
      "sltu %s, %s, %s"
      (reg_to_string rd)
      (reg_to_string rs1)
      (reg_to_string rs2)
  | Sltiu (rd, rs1, imm) ->
    Printf.sprintf "sltiu %s, %s, %d" (reg_to_string rd) (reg_to_string rs1) imm
  | Lw (rd, offset, rs1) ->
    Printf.sprintf "lw %s, %d(%s)" (reg_to_string rd) offset (reg_to_string rs1)
  | Sw (rs2, offset, rs1) ->
    Printf.sprintf "sw %s, %d(%s)" (reg_to_string rs2) offset (reg_to_string rs1)
  | Beq (rs1, rs2, label) ->
    Printf.sprintf "beq %s, %s, %s" (reg_to_string rs1) (reg_to_string rs2) label
  | Bne (rs1, rs2, label) ->
    Printf.sprintf "bne %s, %s, %s" (reg_to_string rs1) (reg_to_string rs2) label
  | Blt (rs1, rs2, label) ->
    Printf.sprintf "blt %s, %s, %s" (reg_to_string rs1) (reg_to_string rs2) label
  | Bge (rs1, rs2, label) ->
    Printf.sprintf "bge %s, %s, %s" (reg_to_string rs1) (reg_to_string rs2) label
  | Ble (rs1, rs2, label) ->
    Printf.sprintf "ble %s, %s, %s" (reg_to_string rs1) (reg_to_string rs2) label
  | Bgt (rs1, rs2, label) ->
    Printf.sprintf "bgt %s, %s, %s" (reg_to_string rs1) (reg_to_string rs2) label
  | J label -> Printf.sprintf "j %s" label
  | Jal (rd, label) -> Printf.sprintf "jal %s, %s" (reg_to_string rd) label
  | Jalr (rd, rs1, offset) ->
    Printf.sprintf "jalr %s, %s, %d" (reg_to_string rd) (reg_to_string rs1) offset
  | Ret -> "ret"
  | Li (rd, imm) -> Printf.sprintf "li %s, %d" (reg_to_string rd) imm
  | Lui (rd, imm) -> Printf.sprintf "lui %s, %d" (reg_to_string rd) imm
  | Mv (rd, rs) -> Printf.sprintf "mv %s, %s" (reg_to_string rd) (reg_to_string rs)
  | Nop -> "nop"
;;

(* 将汇编项转换为字符串 *)
let asm_item_to_string = function
  | Label label -> label ^ ":"
  | Instruction instr -> "    " ^ instruction_to_string instr
  | Comment comment -> "    # " ^ comment
  | Directive directive -> "    " ^ directive
;;

(* 输出汇编代码到文件 *)
let emit_asm_to_file filename asm_items =
  let oc = open_out filename in
  List.iter
    (fun item ->
       output_string oc (asm_item_to_string item);
       output_string oc "\n")
    asm_items;
  close_out oc
;;

(* 输出汇编代码到字符串 *)
let emit_asm_to_string asm_items =
  String.concat "\n" (List.map asm_item_to_string asm_items)
;;
