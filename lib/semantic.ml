(*
  语义分析主模块：
  - 负责遍历 AST，进行符号表管理和各种语义检查。
  - 检查变量/函数声明、类型匹配、作用域、控制流、main 函数等。
  - 检查出错时抛出 Semantic_error 异常。
  - 可为 AST 节点添加类型注解等"注释"。
*)

open Ast
open Symbol

exception Semantic_error of string
exception NotAllPathsReturn of string

(* 语义分析上下文：记录当前函数、循环深度等信息 *)
type context = {
  current_func : (string * return_type) option;  (* 当前函数名和返回类型 *)
  in_loop : int;                                 (* 当前循环嵌套层数 *)
}

(*let empty_context = { current_func = None; in_loop = 0 }*)

(* 检查主程序入口 main 函数是否存在且符合要求 *)
let check_main_function (prog : comp_unit) =
  let found =
    List.exists (fun f ->
      f.name = "main" && f.params = [] && f.ret_type = Int
    ) prog
  in
  if not found then
    raise (Semantic_error "必须有一个名为 main、无参数且返回 int 的主函数")

(* 辅助函数：检查变量/函数是否已声明，未声明则抛出异常 *)
let require_declared name symtab =
  match Symbol.find_symbol name symtab with
  | None -> raise (Semantic_error ("未声明的函数：" ^ name))
  | Some info -> info

(* 辅助函数：检查变量赋值时类型是否匹配 *)
let check_var_assign name expr_typ symtab =
  match Symbol.find_symbol name symtab with
  | Some (Var { typ; _ }) ->
      if typ <> expr_typ then
        raise (Semantic_error ("赋值类型不匹配：" ^ name))
  | Some (Func _) -> raise (Semantic_error ("不能给函数名赋值：" ^ name))
  | None -> raise (Semantic_error ("未声明的变量：" ^ name))

(* 检查 break/continue 是否在循环内 *)
let check_in_loop ctx keyword =
  if ctx.in_loop = 0 then
    raise (Semantic_error (keyword ^ " 语句只能出现在循环内部"))

(* 检查 return 语句类型 *)
let check_return_type ctx expr_opt =
  match ctx.current_func with
  | None -> ()
  | Some (_, ret_type) ->
      (match expr_opt, ret_type with
      | Some e_typ, Int -> if e_typ <> Int then raise (Semantic_error "int 函数必须返回 int")
      | Some _, Void -> raise (Semantic_error "void 函数不能返回值")
      | None, Int -> raise (Semantic_error "int 函数必须返回值")
      | None, Void -> () )

(* 表达式类型推断与检查 *)
let rec infer_expr (symtab : Symbol.t) (ctx : context) (expr : Ast.expr) : Ast.return_type =
  match expr with
  | Int _ -> Int
  | Var name ->
      (match require_declared name symtab with
      | Var { typ; _ } -> typ
      | Func _ -> raise (Semantic_error ("不能将函数名作为变量使用：" ^ name)))
  | UnaryOp (op, e) ->
      let t = infer_expr symtab ctx e in
      (match op with
      | Neg | Not ->
          if t <> Int then raise (Semantic_error "一元运算符操作数类型必须为 int");
          Int)
  | BinaryOp (op, e1, e2) ->
      let t1 = infer_expr symtab ctx e1 in
      let t2 = infer_expr symtab ctx e2 in
      (match op with
      | Add | Sub | Mul | Div | Mod
      | Eq | Neq | Lt | Leq | Gt | Geq
      | And | Or ->
          if t1 <> Int || t2 <> Int then
            raise (Semantic_error "二元运算符操作数类型必须为 int");
          if op = Div || op = Mod then
            (match e2 with Int 0 -> raise (Semantic_error "除数不能为零") | _ -> ());
          Int)
  | Call (fname, args) ->
      (match require_declared fname symtab with
      | Func { ret_type; params; _ } ->
          let param_types = List.map fst params in
          let arg_types = List.map (infer_expr symtab ctx) args in
          if List.length param_types <> List.length arg_types then
            raise (Semantic_error ("函数参数数量不匹配：" ^ fname));
          ignore (List.map2 (fun pt at -> if pt <> at then raise (Semantic_error ("函数参数类型不匹配：" ^ fname))) param_types arg_types);
          ret_type
      | Var _ -> raise (Semantic_error ("不能将变量作为函数调用：" ^ fname)))

(* 将参数列表转换为 (return_type * string) 列表 *)
let params_to_typ_name_list params =
  List.map (function Param name -> (Int, name)) params

(* 语句检查，返回是否所有路径都有 return（用于 int 函数） *)
let rec check_stmt symtab ctx stmt =
  match stmt with
  | Empty -> (symtab, false)
  | Expr e -> ignore (infer_expr symtab ctx e); (symtab, false)
  | Block stmts ->
      let symtab' = Symbol.enter_scope symtab in
      let rec aux sytb ctx has_ret = function
        | [] -> (sytb, has_ret)
        | s :: rest ->
            let (sytb', ret) = check_stmt sytb ctx s in
            (* 如果 check_stmt 抛出异常（如不能给函数名赋值），会直接中断 aux，不会被 return 覆盖 *)
            aux sytb' ctx (has_ret || ret) rest
      in
      let (symtab_out, has_return) = aux symtab' ctx false stmts in
      (Symbol.exit_scope symtab_out, has_return)
  | Return e_opt ->
      let typ_opt = Option.map (infer_expr symtab ctx) e_opt in
      check_return_type ctx typ_opt; (symtab, true)
  | If (cond, s1, Some s2) ->
      let t = infer_expr symtab ctx cond in
      if t <> Int then raise (Semantic_error "if 条件类型必须为 int");
      let (_, r1) = check_stmt symtab ctx s1 in
      let (_, r2) = check_stmt symtab ctx s2 in
      (symtab, r1 && r2)
  | If (cond, s1, None) ->
      let t = infer_expr symtab ctx cond in
      if t <> Int then raise (Semantic_error "if 条件类型必须为 int");
      ignore (check_stmt symtab ctx s1); (symtab, false)
  | While (cond, body) ->
      let t = infer_expr symtab ctx cond in
      if t <> Int then raise (Semantic_error "while 条件类型必须为 int");
      let always_true = match cond with Int 1 -> true | _ -> false in
      let (_, body_ret) = check_stmt symtab { ctx with in_loop = ctx.in_loop + 1 } body in
      if always_true && body_ret then (symtab, true)
      else (symtab, false)
  | Break -> check_in_loop ctx "break"; (symtab, false)
  | Continue -> check_in_loop ctx "continue"; (symtab, false)
  | Declare (name, e) ->
      let typ = infer_expr symtab ctx e in
      if Symbol.is_declared_in_current_scope name symtab then
        raise (Semantic_error ("变量重复声明：" ^ name));
      let symtab' = Symbol.add_symbol name (Var { typ; declared = true; offset = 0 }) symtab in
      (symtab', false)
  | Assign (name, e) ->
      let typ = infer_expr symtab ctx e in
      check_var_assign name typ symtab;
      (symtab, false)

(* 函数定义检查 *)
let check_func symtab fdef =
  let symtab' = Symbol.enter_scope symtab in
  (* 参数加入符号表 *)
  let symtab'' =
    List.fold_left (fun s p ->
      match p with
      | Param name ->
        if Symbol.is_declared_in_current_scope name s then
          raise (Semantic_error ("参数名重复：" ^ name));
        Symbol.add_symbol name (Var { typ = Int; declared = true; offset = 0 }) s
    ) symtab' fdef.params
  in
  let ctx = { current_func = Some (fdef.name, fdef.ret_type); in_loop = 0 } in
  let (_, has_return) = check_stmt symtab'' ctx fdef.body in
  (* int 函数所有路径必须 return *)
  if fdef.ret_type = Int && not has_return then
    raise (NotAllPathsReturn ("int 类型函数 " ^ fdef.name ^ " 不是所有路径都返回值"))

(* 程序主入口 *)
let analyze_program (prog : comp_unit) =
  check_main_function prog;
  (* 构建全局符号表，检查函数名唯一性 *)
  let symtab = Symbol.enter_scope [] in
  let symtab =
    List.fold_left (fun s f ->
      if Symbol.is_declared_in_current_scope f.name s then
        raise (Semantic_error ("重复定义的函数名：" ^ f.name));
      Symbol.add_symbol f.name (Func { ret_type = f.ret_type; params = params_to_typ_name_list f.params; declared = true }) s
    ) symtab prog
  in
  List.iter (check_func symtab) prog 