{
(*
  OCaml 头代码部分。
  我们需要打开 Parser 模块，这样才能使用其中定义的 token 类型 (如 IF, PLUS, ID 等)。
*)
open Parser
}

(* 定义一些常用的正则表达式别名 *)
let digit = ['0'-'9']
let letter = ['a'-'z' 'A'-'Z']
let ident = (letter | '_') (letter | digit | '_')*

rule tokenize = parse
  (* 规则的顺序很重要，ocamllex 会选择第一个匹配的规则 *)

  | [' ' '\t' '\r' '\n']  { tokenize lexbuf }     (* 1. 忽略空白字符 *)
  | "/*"                  { comment lexbuf }      (* 2. 匹配多行注释 *)
  | "//" [^ '\n']* { tokenize lexbuf }     (* 3. 匹配单行注释 *)

  (* 4. 关键字 *)
  | "if"        { IF }
  | "else"      { ELSE }
  | "while"     { WHILE }
  | "break"     { BREAK }
  | "continue"  { CONTINUE }
  | "return"    { RETURN }
  | "int"       { INT }
  | "void"      { VOID }

  (* 5. 标识符: 必须放在关键字之后 *)
  | ident as id { ID id }

  (* 6. 整数常量 *)
  | digit+ as n { NUMBER (int_of_string n) }

  (* 7. 运算符和分隔符 *)
  | "+"         { PLUS }
  | "-"         { MINUS }
  | "*"         { STAR }
  | "/"         { SLASH }
  | "%"         { MOD }
  | "=="        { EQ }
  | "!="        { NEQ }
  | "<"         { LT }
  | "<="        { LEQ }
  | ">"         { GT }
  | ">="        { GEQ }
  | "&&"        { AND }
  | "||"        { OR }
  | "!"         { NOT }
  | "="         { ASSIGN }
  | ";"         { SEMI }
  | ","         { COMMA }
  | "("         { LPAREN }
  | ")"         { RPAREN }
  | "{"         { LBRACE }
  | "}"         { RBRACE }

  (* 8. 文件结束符 *)
  | eof         { EOF }

(* C 风格的多行注释处理 *)
and comment = parse
  | "*/" { tokenize lexbuf }
  | _    { comment lexbuf }
