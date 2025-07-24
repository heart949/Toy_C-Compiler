(* print_ir.ml *)

open Ir

let string_of_operand = function
  | Reg name -> name
  | Imm i -> string_of_int i
  | Var name -> name

let string_of_operands ops = String.concat ", " (List.map string_of_operand ops)

let string_of_ir_inst = function
  | Binop (op, dst, lhs, rhs) ->
      Printf.sprintf "%s = %s %s %s" (string_of_operand dst)
        (string_of_operand lhs) op (string_of_operand rhs)
  | Unop (op, dst, src) ->
      Printf.sprintf "%s = %s%s" (string_of_operand dst) op
        (string_of_operand src)
  | Load (dst, src) ->
      Printf.sprintf "%s = *%s" (string_of_operand dst) (string_of_operand src)
  | Store (dst, src) ->
      Printf.sprintf "*%s = %s" (string_of_operand dst) (string_of_operand src)
  | Goto label -> Printf.sprintf "goto %s" label
  | IfGoto (cond, label) ->
      Printf.sprintf "if %s goto %s" (string_of_operand cond) label
  | Label name -> Printf.sprintf "%s:" name
  | Call (ret, fname, args) ->
      Printf.sprintf "%s = call %s(%s)" (string_of_operand ret) fname
        (string_of_operands args)
  | Ret None -> "return"
  | Ret (Some op) -> Printf.sprintf "return %s" (string_of_operand op)
  | Assign (dst, src) ->
      Printf.sprintf "%s = %s" (string_of_operand dst) (string_of_operand src)

let print_ir_func (f : ir_func) =
  Printf.printf "function %s(%s):\n" f.name (String.concat ", " f.args);
  List.iter (fun inst -> Printf.printf "  %s\n" (string_of_ir_inst inst)) f.body

let print_ir_program (prog : ir_program) = List.iter print_ir_func prog
