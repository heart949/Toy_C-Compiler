open Ir

let stack_offset = ref 0
let var_env = Hashtbl.create 64

let alloc_stack var =
  stack_offset := !stack_offset - 4;
  Hashtbl.add var_env var !stack_offset;
  !stack_offset

let get_stack_offset var =
  try Hashtbl.find var_env var
  with Not_found -> failwith ("Unknown variable: " ^ var)

let operand_to_str = function
  | Reg r | Var r -> Printf.sprintf "%d(sp)" (get_stack_offset r)
  | Imm i -> Printf.sprintf "%d" i

let load_operand (reg : string) (op : operand) : string =
  match op with
  | Imm i -> Printf.sprintf "\tli %s, %d\n" reg i
  | Reg r | Var r -> Printf.sprintf "\tlw %s, %d(sp)\n" reg (get_stack_offset r)

let compile_inst (inst : ir_inst) : string =
  match inst with
  | Binop (op, dst, lhs, rhs) ->
      let dst_off =
        alloc_stack
          (match dst with Reg r | Var r -> r | _ -> failwith "Bad dst")
      in
      let lhs_code = load_operand "t1" lhs in
      let rhs_code = load_operand "t2" rhs in
      let op_code =
        match op with
        | "+" -> "\tadd t0, t1, t2\n"
        | "-" -> "\tsub t0, t1, t2\n"
        | "*" -> "\tmul t0, t1, t2\n"
        | "/" -> "\tdiv t0, t1, t2\n"
        | "%" -> "\trem t0, t1, t2\n"
        (* TODO 实现 *)
        | "==" | "!=" | "<=" | ">=" ->
            "\tsub t0, t1, t2\n\tseqz t0, t0\n" (* 占位实现 *)
        | "<" -> "\tslt t0, t1, t2\n"
        | ">" -> "\tsgt t0, t1, t2\n"
        | "&&" -> "\tand t0, t1, t2\n"
        | "||" -> "\tor t0, t1, t2\n"
        | _ -> failwith ("Unknown binop: " ^ op)
      in
      lhs_code ^ rhs_code ^ op_code ^ Printf.sprintf "\tsw t0, %d(sp)\n" dst_off
  | Unop (op, dst, src) ->
      let dst_off =
        alloc_stack
          (match dst with Reg r | Var r -> r | _ -> failwith "Bad dst")
      in
      let load_src = load_operand "t1" src in
      let op_code =
        match op with
        | "-" -> "\tneg t0, t1\n"
        | "!" -> "\tseqz t0, t1\n"
        | "+" -> "\tmv t0, t1\n"
        | _ -> failwith ("Unknown unop: " ^ op)
      in
      load_src ^ op_code ^ Printf.sprintf "\tsw t0, %d(sp)\n" dst_off
  | Assign (dst, src) ->
      let dst_off =
        alloc_stack
          (match dst with Reg r | Var r -> r | _ -> failwith "Bad dst")
      in
      let load_src = load_operand "t0" src in
      load_src ^ Printf.sprintf "\tsw t0, %d(sp)\n" dst_off
  | Load (dst, src) ->
      let dst_off =
        alloc_stack
          (match dst with Reg r | Var r -> r | _ -> failwith "Bad dst")
      in
      let src_code = load_operand "t1" src in
      src_code ^ "\tlw t0, 0(t1)\n" ^ Printf.sprintf "\tsw t0, %d(sp)\n" dst_off
  | Store (dst, src) ->
      let dst_code = load_operand "t1" dst in
      let src_code = load_operand "t2" src in
      dst_code ^ src_code ^ "\tsw t2, 0(t1)\n"
  | Call (dst, fname, args) ->
      let dst_off =
        alloc_stack
          (match dst with Reg r | Var r -> r | _ -> failwith "Bad dst")
      in
      let args_code =
        List.mapi (fun i arg -> load_operand (Printf.sprintf "a%d" i) arg) args
        |> String.concat ""
      in
      args_code ^ Printf.sprintf "\tcall %s\n\tsw a0, %d(sp)\n" fname dst_off
  | Ret None -> "\taddi sp, sp, 256\n\tret\n"
  | Ret (Some op) ->
      let load_code = load_operand "a0" op in
      load_code ^ "\taddi sp, sp, 256\n\tret\n"
  | Goto label -> Printf.sprintf "\tj %s\n" label
  | IfGoto (cond, label) ->
      let cond_code = load_operand "t0" cond in
      cond_code ^ Printf.sprintf "\tbne t0, x0, %s\n" label
  | Label label -> Printf.sprintf "%s:\n" label

let compile_func (f : ir_func) : string =
  Hashtbl.clear var_env;
  stack_offset := 0;

  (* 参数入栈 *)
  let param_setup =
    List.mapi
      (fun i name ->
        let off = alloc_stack name in
        Printf.sprintf "\tsw a%d, %d(sp)\n" i off)
      f.args
    |> String.concat ""
  in

  let body_code = f.body |> List.map compile_inst |> String.concat "" in
  let func_label = f.name in
  let prologue = Printf.sprintf "%s:\n\taddi sp, sp, -256\n" func_label in
  prologue ^ param_setup ^ body_code

let compile_program (prog : ir_program) : string =
  ".text\n.global main\n\n" ^ (List.map compile_func prog |> String.concat "\n")
