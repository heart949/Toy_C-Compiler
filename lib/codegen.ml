(*
   ToyC 编译器代码生成模块
  将 AST 翻译为 RISC-V 32位汇编代码
*)

open Ast
open Riscv

(* 代码生成上下文 *)
type codegen_context =
  { mutable label_counter : int (* 标签计数器 *)
  ; mutable temp_counter : int (* 临时寄存器计数器 *)
  ; mutable stack_offset : int (* 当前栈偏移 *)
  ; mutable break_labels : string list (* break 跳转标签栈 *)
  ; mutable continue_labels : string list (* continue 跳转标签栈 *)
  ; mutable local_vars : (string * int) list (* 局部变量映射到栈偏移 *)
  }

(* 创建新的代码生成上下文 *)
let create_context _symbol_table =
  { label_counter = 0
  ; temp_counter = 0
  ; stack_offset = 0 (* fp-based offset, starts from 0 and goes down *)
  ; break_labels = []
  ; continue_labels = []
  ; local_vars = []
  }
;;

(* 生成新标签 *)
let new_label ctx prefix =
  let label = Printf.sprintf "%s%d" prefix ctx.label_counter in
  ctx.label_counter <- ctx.label_counter + 1;
  label
;;

(* 获取临时寄存器 *)
let get_temp_reg ctx =
  let reg =
    match ctx.temp_counter mod 7 with
    (* T0-T6 *)
    | 0 -> T0
    | 1 -> T1
    | 2 -> T2
    | 3 -> T3
    | 4 -> T4
    | 5 -> T5
    | 6 -> T6
    | _ -> failwith "Should not happen"
  in
  ctx.temp_counter <- ctx.temp_counter + 1;
  reg
;;

(* 将变量添加到栈中 *)
let add_local_var ctx name =
  ctx.stack_offset <- ctx.stack_offset - 4;
  ctx.local_vars <- (name, ctx.stack_offset) :: ctx.local_vars;
  ctx.stack_offset
;;

(* 获取变量的栈偏移 *)
let get_var_offset ctx name =
  try List.assoc name ctx.local_vars with
  | Not_found -> failwith ("Variable not found: " ^ name)
;;

(* 生成表达式代码，返回结果寄存器和指令列表 *)
let rec gen_expr ctx (expr : Ast.expr) : reg * instruction list =
  match expr with
  | Ast.Int n ->
    let reg = get_temp_reg ctx in
    let instr = [ Li (reg, n) ] in
    reg, instr
  | Ast.Var name ->
    let reg = get_temp_reg ctx in
    let offset = get_var_offset ctx name in
    let instr = [ Lw (reg, offset, Fp) ] in
    reg, instr
  | Ast.UnaryOp (op, e) ->
    let e_reg, e_instrs = gen_expr ctx e in
    let result_reg = get_temp_reg ctx in
    let instrs =
      match op with
      | Ast.Neg -> e_instrs @ [ Sub (result_reg, Zero, e_reg) ]
      | Ast.Not -> e_instrs @ [ Sltiu (result_reg, e_reg, 1) ]
    in
    result_reg, instrs
  | Ast.BinaryOp (op, e1, e2) ->
    let e1_reg, e1_instrs = gen_expr ctx e1 in
    let e2_reg, e2_instrs = gen_expr ctx e2 in
    let result_reg = get_temp_reg ctx in
    let op_instrs =
      match op with
      | Ast.Add -> [ Add (result_reg, e1_reg, e2_reg) ]
      | Ast.Sub -> [ Sub (result_reg, e1_reg, e2_reg) ]
      | Ast.Mul -> [ Mul (result_reg, e1_reg, e2_reg) ]
      | Ast.Div -> [ Div (result_reg, e1_reg, e2_reg) ]
      | Ast.Mod -> [ Rem (result_reg, e1_reg, e2_reg) ]
      | Ast.Eq -> [ Sub (result_reg, e1_reg, e2_reg); Sltiu (result_reg, result_reg, 1) ]
      | Ast.Neq ->
        [ Sub (result_reg, e1_reg, e2_reg); Sltu (result_reg, Zero, result_reg) ]
      | Ast.Lt -> [ Slt (result_reg, e1_reg, e2_reg) ]
      | Ast.Leq -> [ Slt (result_reg, e2_reg, e1_reg); Xori (result_reg, result_reg, 1) ]
      | Ast.Gt -> [ Slt (result_reg, e2_reg, e1_reg) ]
      | Ast.Geq -> [ Slt (result_reg, e1_reg, e2_reg); Xori (result_reg, result_reg, 1) ]
      | Ast.And ->
        (* This is a shortcut, not a full logical AND with short-circuiting *)
        [ Sltu (T0, Zero, e1_reg); Sltu (T1, Zero, e2_reg); And (result_reg, T0, T1) ]
      | Ast.Or ->
        (* This is a shortcut, not a full logical OR with short-circuiting *)
        [ Or (result_reg, e1_reg, e2_reg); Sltu (result_reg, Zero, result_reg) ]
    in
    let instrs = e1_instrs @ e2_instrs @ op_instrs in
    result_reg, instrs
  | Ast.Call (fname, args) ->
    let result_reg = A0 in
    let arg_instrs =
      List.mapi
        (fun i arg ->
           let arg_reg, arg_code = gen_expr ctx arg in
           let target_reg =
             match i with
             | 0 -> A0
             | 1 -> A1
             | 2 -> A2
             | 3 -> A3
             | 4 -> A4
             | 5 -> A5
             | 6 -> A6
             | 7 -> A7
             | _ -> failwith "Too many arguments"
           in
           arg_code @ [ Mv (target_reg, arg_reg) ])
        args
      |> List.flatten
    in
    let call_instr = [ Jal (Ra, fname) ] in
    result_reg, arg_instrs @ call_instr
;;

(* Generate epilogue *)
let gen_epilogue_instrs frame_size =
  [ (* First restore registers from stack before releasing stack frame *)
    Lw (Ra, frame_size - 4, Sp) (* Restore return address *)
  ; Lw (Fp, frame_size - 8, Sp) (* Restore old frame pointer *)
  ; Addi (Sp, Sp, frame_size) (* Release stack frame *)
  ; Ret (* Return to caller *)
  ]
;;

(* 生成语句代码 *)
let rec gen_stmt ctx frame_size (stmt : Ast.stmt) : asm_item list =
  match stmt with
  | Ast.Empty -> []
  | Ast.Expr e ->
    let _, instrs = gen_expr ctx e in
    List.map (fun i -> Instruction i) instrs
  | Ast.Block stmts ->
    let old_vars = ctx.local_vars in
    let old_offset = ctx.stack_offset in
    let items = List.map (gen_stmt ctx frame_size) stmts |> List.flatten in
    ctx.local_vars <- old_vars;
    ctx.stack_offset <- old_offset;
    items
  | Ast.Return (Some e) ->
    (* Optimize for simple constant 0 *)
    (match e with
     | Ast.Int 0 ->
       let all_instrs = [ Li (A0, 0) ] @ gen_epilogue_instrs frame_size in
       List.map (fun i -> Instruction i) all_instrs
     | _ ->
       let e_reg, e_instrs = gen_expr ctx e in
       let all_instrs = e_instrs @ [ Mv (A0, e_reg) ] @ gen_epilogue_instrs frame_size in
       List.map (fun i -> Instruction i) all_instrs)
  | Ast.Return None -> List.map (fun i -> Instruction i) (gen_epilogue_instrs frame_size)
  | Ast.If (cond, then_stmt, else_stmt) ->
    let cond_reg, cond_instrs = gen_expr ctx cond in
    let else_label = new_label ctx "else" in
    let end_label = new_label ctx "endif" in
    let then_items = gen_stmt ctx frame_size then_stmt in
    let else_items =
      match else_stmt with
      | Some s -> gen_stmt ctx frame_size s
      | None -> []
    in
    List.map (fun i -> Instruction i) cond_instrs
    @ [ Instruction (Beq (cond_reg, Zero, else_label)) ]
    @ then_items
    @ [ Instruction (J end_label); Label else_label ]
    @ else_items
    @ [ Label end_label ]
  | Ast.While (cond, body) ->
    let loop_label = new_label ctx "loop" in
    let end_label = new_label ctx "endloop" in
    ctx.break_labels <- end_label :: ctx.break_labels;
    ctx.continue_labels <- loop_label :: ctx.continue_labels;
    let cond_reg, cond_instrs = gen_expr ctx cond in
    let body_items = gen_stmt ctx frame_size body in
    ctx.break_labels <- List.tl ctx.break_labels;
    ctx.continue_labels <- List.tl ctx.continue_labels;
    [ Label loop_label ]
    @ List.map (fun i -> Instruction i) cond_instrs
    @ [ Instruction (Beq (cond_reg, Zero, end_label)) ]
    @ body_items
    @ [ Instruction (J loop_label); Label end_label ]
  | Ast.Break ->
    (match ctx.break_labels with
     | label :: _ -> [ Instruction (J label) ]
     | [] -> failwith "Break outside loop")
  | Ast.Continue ->
    (match ctx.continue_labels with
     | label :: _ -> [ Instruction (J label) ]
     | [] -> failwith "Continue outside loop")
  | Ast.Declare (name, e) ->
    let offset = add_local_var ctx name in
    let e_reg, e_instrs = gen_expr ctx e in
    let all_instrs = e_instrs @ [ Sw (e_reg, offset, Fp) ] in
    List.map (fun i -> Instruction i) all_instrs
  | Ast.Assign (name, e) ->
    let offset = get_var_offset ctx name in
    let e_reg, e_instrs = gen_expr ctx e in
    let all_instrs = e_instrs @ [ Sw (e_reg, offset, Fp) ] in
    List.map (fun i -> Instruction i) all_instrs
;;

(* 计算函数所需的栈帧大小 *)
let calculate_frame_size (func_def : Ast.func_def) =
  (* Helper to count declarations in a statement *)
  let rec count_decls_in_stmt stmt =
    match stmt with
    | Declare _ -> 1
    | Block stmts -> List.fold_left (fun acc s -> acc + count_decls_in_stmt s) 0 stmts
    | If (_, s1, Some s2) -> count_decls_in_stmt s1 + count_decls_in_stmt s2
    | If (_, s1, None) -> count_decls_in_stmt s1
    | While (_, s) -> count_decls_in_stmt s
    | _ -> 0
  in
  let num_locals = count_decls_in_stmt func_def.body in
  let num_params = List.length func_def.params in
  (* ra, fp + params + locals *)
  let required_space = 8 + (num_params * 4) + (num_locals * 4) in
  (* Align to 16 bytes *)
  (required_space + 15) / 16 * 16
;;

(* 生成函数代码 *)
let gen_function symbol_table (func_def : Ast.func_def) : asm_item list =
  let ctx = create_context symbol_table in
  let frame_size = calculate_frame_size func_def in
  (* 函数序言 *)
  let prologue =
    [ Comment "prologue"
    ; Instruction (Addi (Sp, Sp, -frame_size))
    ; Instruction (Sw (Ra, frame_size - 4, Sp))
    ; Instruction (Sw (Fp, frame_size - 8, Sp))
    ; Instruction (Addi (Fp, Sp, frame_size))
    ]
  in
  (* 处理参数 *)
  ctx.stack_offset <- 0;
  (* Start allocating locals below fp *)
  let param_instrs =
    List.mapi
      (fun i param ->
         match param with
         | Ast.Param name ->
           let offset = add_local_var ctx name in
           let arg_reg =
             match i with
             | 0 -> A0
             | 1 -> A1
             | 2 -> A2
             | 3 -> A3
             | 4 -> A4
             | 5 -> A5
             | 6 -> A6
             | 7 -> A7
             | _ -> failwith "Too many parameters"
           in
           [ Instruction (Sw (arg_reg, offset, Fp)) ])
      func_def.params
    |> List.flatten
  in
  (* 函数体 *)
  let body_items = gen_stmt ctx frame_size func_def.body in
  (* 函数尾声（如果函数没有显式 return） *)
  let epilogue =
    let has_ret =
      List.exists
        (function
          | Instruction Ret -> true
          | _ -> false)
        body_items
    in
    if has_ret
    then []
    else List.map (fun i -> Instruction i) (gen_epilogue_instrs frame_size)
  in
  prologue @ param_instrs @ body_items @ epilogue
;;

(* 生成程序代码 *)
let gen_program symbol_table (program : Ast.comp_unit) =
  (* 全局声明 *)
  let header =
    [ Directive ".text"; Directive ".globl main"; Comment "ToyC Compiler Generated Code" ]
  in
  (* 生成所有函数 *)
  let func_asm_items =
    List.map
      (fun func_def ->
         let items = gen_function symbol_table func_def in
         [ Label func_def.name; Comment ("Function: " ^ func_def.name) ] @ items)
      program
    |> List.flatten
  in
  header @ func_asm_items
;;

(* 主入口函数：编译程序并输出汇编文件 *)
let compile_to_riscv symbol_table program output_file =
  let asm_items = gen_program symbol_table program in
  Riscv.emit_asm_to_file output_file asm_items
;;
