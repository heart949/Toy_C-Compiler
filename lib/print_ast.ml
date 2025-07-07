open Ast

let string_of_typ = function TInt -> "int" | TVoid -> "void"

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

let rec string_of_expr = function
  | Number n -> Printf.sprintf "Number(%d)" n
  | ID s -> Printf.sprintf "ID(%s)" s
  | Call (fname, args) ->
      Printf.sprintf "Call(%s, [%s])" fname
        (String.concat "; " (List.map string_of_expr args))
  | Binop (op, e1, e2) ->
      Printf.sprintf "Binop(%s, %s, %s)" (string_of_binop op)
        (string_of_expr e1) (string_of_expr e2)
  | Unop (op, e) ->
      Printf.sprintf "Unop(%s, %s)" (string_of_unop op) (string_of_expr e)

let rec string_of_stmt = function
  | Block stmts ->
      Printf.sprintf "Block([\n  %s\n])"
        (String.concat ";\n  " (List.map string_of_stmt stmts))
  | Empty -> "Empty"
  | ExprStmt e -> Printf.sprintf "ExprStmt(%s)" (string_of_expr e)
  | Assign (id, e) -> Printf.sprintf "Assign(%s, %s)" id (string_of_expr e)
  | Decl (id, None) -> Printf.sprintf "Decl(%s)" id
  | Decl (id, Some e) -> Printf.sprintf "Decl(%s, %s)" id (string_of_expr e)
  | If (cond, s1, None) ->
      Printf.sprintf "If(%s, %s)" (string_of_expr cond) (string_of_stmt s1)
  | If (cond, s1, Some s2) ->
      Printf.sprintf "If(%s, %s, %s)" (string_of_expr cond) (string_of_stmt s1)
        (string_of_stmt s2)
  | While (cond, body) ->
      Printf.sprintf "While(%s, %s)" (string_of_expr cond) (string_of_stmt body)
  | Break -> "Break"
  | Continue -> "Continue"
  | Return None -> "Return"
  | Return (Some e) -> Printf.sprintf "Return(%s)" (string_of_expr e)

let string_of_func_def f =
  Printf.sprintf "Function %s(%s) : %s {\n%s\n}" f.func_name
    (String.concat ", " f.params)
    (string_of_typ f.ret_type)
    (String.concat "\n" (List.map (fun s -> "  " ^ string_of_stmt s) f.body))

let string_of_comp_unit (unit : comp_unit) : string =
  String.concat "\n\n" (List.map string_of_func_def unit)
