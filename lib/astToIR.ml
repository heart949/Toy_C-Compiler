(*astToIR.ml*)
(* 引入 AST 和 IR 类型 *)
open Ast
open Ir

(* Map<String, operand> *)
module Env = Map.Make (String)

(* 翻译上下文：包含当前 env 及循环标签，用于实现 break/continue *)
type context = {
  env : operand Env.t;
  break_lbl : string option; (* break 跳转目标 *)
  continue_lbl : string option; (* continue 跳转目标 *)
}

(* 临时寄存器与标签生成器 *)
let temp_id = ref 0

let fresh_temp () =
  let id = !temp_id in
  incr temp_id;
  Reg ("t" ^ string_of_int id)

let label_id = ref 0

let fresh_label () =
  let id = !label_id in
  incr label_id;
  "L" ^ string_of_int id

(* 操作符到字符串映射 *)
let string_of_binop = function
  | Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Div -> "/"
  | Mod -> "%"
  | Eq -> "=="
  | Neq -> "!="
  | Less -> "<"
  | Leq -> "<="
  | Greater -> ">"
  | Geq -> ">="
  | Land -> "&&"
  | Lor -> "||"

let string_of_unop = function Not -> "!" | Plus -> "+" | Minus -> "-"

(* stmt_to_res 用于处理 return 终止：Normal/Returned 两种结果 *)
type stmt_res = Normal of ir_inst list | Returned of ir_inst list

(* 将 stmt_res 展平为代码列表 *)
let flatten = function Normal code | Returned code -> code

(* 检查代码段最后一条是否是 Goto 指定标签或 Return *)
let ends_with_jump_or_return insts =
  match List.rev insts with
  | Goto _ :: _ -> true
  | Ret _ :: _ -> true
  | _ -> false

(* 表达式转换：返回目标寄存器和 IR 指令列表 *)
let rec expr_to_ir (ctx : context) (e : expr) : operand * ir_inst list =
  match e with
  | Number n -> (Imm n, [])
  | ID name ->
      let operand = Env.find name ctx.env in
      (operand, [])
  | Unop (op, e1) ->
      let operand, code = expr_to_ir ctx e1 in
      let res = fresh_temp () in
      (res, code @ [ Unop (string_of_unop op, res, operand) ])
  | Binop (Land, e1, e2) ->
      (* 短路与: a && b *)
      let lhs, c1 = expr_to_ir ctx e1 in
      let res = fresh_temp () in
      let l_false = fresh_label () in
      let l_end = fresh_label () in
      (* 1. 如果 lhs==0 跳到 false *)
      let code =
        c1
        @ [ IfGoto (lhs, l_false) ]
        (* 2. 否则计算 rhs *)
        @ (let rhs, c2 = expr_to_ir ctx e2 in
           c2 @ [ Assign (res, rhs) ])
        (* 3. 跳到结束 *)
        @ [ Goto l_end; Label l_false; Assign (res, Imm 0); Label l_end ]
      in
      (res, code)
  | Binop (Lor, e1, e2) ->
      (* 短路或: a || b *)
      let lhs, c1 = expr_to_ir ctx e1 in
      let res = fresh_temp () in
      let l_true = fresh_label () in
      let l_end = fresh_label () in
      let code =
        c1
        (* 如果 lhs != 0 跳到 true *)
        @ [ IfGoto (lhs, l_true) ]
        (* 否则计算 rhs *)
        @ (let rhs, c2 = expr_to_ir ctx e2 in
           c2 @ [ Assign (res, rhs) ])
        @ [ Goto l_end; Label l_true; Assign (res, Imm 1); Label l_end ]
      in
      (res, code)
  | Binop (op, e1, e2) ->
      (* 普通二元运算 *)
      let lhs, c1 = expr_to_ir ctx e1 in
      let rhs, c2 = expr_to_ir ctx e2 in
      let dst = fresh_temp () in
      (dst, c1 @ c2 @ [ Binop (string_of_binop op, dst, lhs, rhs) ])
  | Call (f, args) ->
      (* 参数顺序按出现顺序计算 *)
      let codes, ops =
        List.fold_left
          (fun (acc_c, acc_o) arg ->
            let o, c = expr_to_ir ctx arg in
            (acc_c @ c, acc_o @ [ o ]))
          ([], []) args
      in
      let ret = fresh_temp () in
      (ret, codes @ [ Call (ret, f, ops) ])

(* 语句翻译，返回 Normal/Returned，支持块作用域、break/continue、return 提前终止 *)
let rec stmt_to_res (ctx : context) (s : stmt) : stmt_res =
  match s with
  | Empty -> Normal []
  | ExprStmt e ->
      let _, code = expr_to_ir ctx e in
      Normal code
  | Decl (_x, None) ->
      (* 无初始化的声明仅更新 env *)
      Normal []
  | Decl (x, Some e) ->
      let v, c = expr_to_ir ctx e in
      let var = Var x in
      Normal (c @ [ Assign (var, v) ])
  | Assign (x, e) ->
      let v, c = expr_to_ir ctx e in
      let var = Env.find x ctx.env in
      Normal (c @ [ Assign (var, v) ])
  | Return None -> Returned [ Ret None ]
  | Return (Some e) ->
      let v, c = expr_to_ir ctx e in
      Returned (c @ [ Ret (Some v) ])
  | If (cond, tstmt, Some fstmt) -> (
      let cnd, cc = expr_to_ir ctx cond in
      let lthen = fresh_label ()
      and lelse = fresh_label ()
      and lend = fresh_label () in
      let then_res = stmt_to_res ctx tstmt
      and else_res = stmt_to_res ctx fstmt in
      let raw_then = flatten then_res in
      let then_code =
        if ends_with_jump_or_return raw_then then raw_then
        else raw_then @ [ Goto lend ]
      in
      let raw_else = flatten else_res in
      let else_code =
        if ends_with_jump_or_return raw_else then raw_else
        else raw_else @ [ Goto lend ]
      in
      let code =
        cc
        @ [ IfGoto (cnd, lthen); Goto lelse ]
        @ [ Label lthen ] @ then_code @ [ Label lelse ] @ else_code
        @ [ Label lend ]
      in
      match (then_res, else_res) with
      | Returned _, _ | _, Returned _ -> Returned code
      | _ -> Normal code)
  | If (cond, tstmt, None) -> (
      let cnd, cc = expr_to_ir ctx cond in
      let lthen = fresh_label () and lskip = fresh_label () in
      let then_res = stmt_to_res ctx tstmt in
      let then_code = flatten then_res in
      let code =
        cc
        @ [ IfGoto (cnd, lthen); Goto lskip ]
        @ [ Label lthen ] @ then_code @ [ Label lskip ]
      in
      match then_res with Returned _ -> Returned code | _ -> Normal code)
  | While (cond, body) ->
      (* 循环标签 *)
      let lcond = fresh_label ()
      and lbody = fresh_label ()
      and lend = fresh_label () in
      let ctx_loop =
        { ctx with break_lbl = Some lend; continue_lbl = Some lcond }
      in
      let cnd, ccode = expr_to_ir ctx_loop cond in
      let body_res = stmt_to_res ctx_loop body in
      let bcode = flatten body_res in
      let code =
        [ Goto lcond; Label lcond ]
        @ ccode
        @ [ IfGoto (cnd, lbody); Goto lend ]
        @ [ Label lbody ] @ bcode @ [ Goto lcond; Label lend ]
      in
      (* 无法从循环体中直接 return：若想支持可在 body_res 捕获 *)
      Normal code
  | Break -> (
      match ctx.break_lbl with
      | Some lbl -> Normal [ Goto lbl ]
      | None -> failwith "break used outside loop")
  | Continue -> (
      match ctx.continue_lbl with
      | Some lbl -> Normal [ Goto lbl ]
      | None -> failwith "continue used outside loop")
  | Block stmts ->
      (* 块作用域隔离 *)
      let saved_env = ctx.env in
      let rec loop env_acc acc_code = function
        | [] -> Normal acc_code
        | hd :: tl -> (
            let ctx' = { ctx with env = env_acc } in
            let res = stmt_to_res ctx' hd in
            let code_hd = flatten res in
            let env_next =
              match hd with
              | Decl (x, _) -> Env.add x (Var x) env_acc
              | _ -> env_acc
            in
            match res with
            | Normal _ -> loop env_next (acc_code @ code_hd) tl
            | Returned _ -> Returned (acc_code @ code_hd))
      in
      (* 执行块体 *)
      let res = loop saved_env [] stmts in
      (* 恢复外层 env *)
      (* ctx.env <- saved_env *)
      res

(* 函数转换 *)
let func_to_ir (f : func_def) : ir_func =
  (* 初始化 env: 参数映射 *)
  let init_env =
    List.fold_left (fun m x -> Env.add x (Var x) m) Env.empty f.params
  in
  let ctx0 = { env = init_env; break_lbl = None; continue_lbl = None } in
  (* 翻译函数体 *)
  let body_res = stmt_to_res ctx0 (Block f.body) in
  let body_code = flatten body_res in
  { name = f.func_name; args = f.params; body = body_code }

(* 编译单元转换 *)
let program_to_ir (cu : comp_unit) : ir_program = List.map func_to_ir cu
