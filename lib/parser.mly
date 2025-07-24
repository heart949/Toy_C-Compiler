/* 1. OCaml 头代码: 定义 token 的数据类型 */
%{
  (*
    这里可以放任意 OCaml 代码。
    目前为空，但未来可以用来放辅助函数。
  *)
  open Ast
%}

/* 2. 定义 Token */

/* 文件结束符 */
%token EOF

/* 关键字 */
%token IF ELSE WHILE BREAK CONTINUE RETURN
%token INT VOID

/* 标识符和字面量 */
%token <string> ID
%token <int> NUMBER

/* 运算符 */
%token PLUS MINUS STAR SLASH MOD
%token EQ NEQ LT LEQ GT GEQ
%token AND OR NOT

/* 括号和分隔符 */
%token LPAREN RPAREN LBRACE RBRACE
%token SEMI COMMA ASSIGN

/* 3. 定义优先级和结合性 (为未来的语法分析做准备) */
%right ASSIGN
%left OR
%left AND
%left EQ NEQ
%left LT LEQ GT GEQ
%left PLUS MINUS
%left STAR SLASH MOD
%right NOT


/* 4. 定义开始符号 (未来语法分析的入口) */
%start <Ast.comp_unit> program

%%

/* 5. 语法规则 (现在可以留空，下一步再填充) */

/*
  program: 编译单元，入口符号，对应一个或多个函数定义
*/
program:
  funcdefs EOF { $1 }
;

/*
  funcdefs: 一个或多个函数定义
*/
funcdefs:
  funcdefs funcdef { $1 @ [$2] }
| funcdef           { [$1] }
;

/*
  funcdef: ToyC 的函数定义
*/
funcdef:
  rettype id=ID LPAREN params_opt RPAREN body=block
    {
      let rec collect_locals stmt =
        match stmt with
        | Declare (id, e) -> [(id, e)]
        | Block stmts -> List.concat_map collect_locals stmts
        | If (_, s1, Some s2) -> collect_locals s1 @ collect_locals s2
        | If (_, s1, None) -> collect_locals s1
        | While (_, s) -> collect_locals s
        | _ -> []
      in
      let locals = collect_locals body in
      { ret_type = $1; name = id; params = $4; body = body; locals = locals }
    }
;

rettype:
  INT  { Int }
| VOID { Void }
;

params_opt:
  params { $1 }
|        { [] }
;

params:
  param                { [$1] }
| params COMMA param   { $1 @ [$3] }
;

param:
  INT id=ID { Param id }
;

/*
  block: 语句块，对应 AST 的 Block
*/
block:
  LBRACE stmts RBRACE { Block $2 }
;

stmts:
  stmts stmt { $1 @ [$2] }
|            { [] }
;

/*
  stmt: ToyC 所有语句类型
  包含：
    - block
    - 空语句
    - 表达式语句
    - 变量赋值
    - 变量声明
    - if/else
    - while
    - break/continue
    - return ;
    - return Expr ;
*/
stmt:
  block                { $1 }
| SEMI                 { Empty }
| expr SEMI            { Expr $1 }
| id=ID ASSIGN e=expr SEMI { Assign (id, e) }
| INT id=ID ASSIGN e=expr SEMI { Declare (id, e) }
| IF LPAREN cond=expr RPAREN s1=stmt ELSE s2=stmt { If (cond, s1, Some s2) }
| IF LPAREN cond=expr RPAREN s1=stmt              { If (cond, s1, None) }
| WHILE LPAREN cond=expr RPAREN body=stmt         { While (cond, body) }
| BREAK SEMI                                      { Break }
| CONTINUE SEMI                                   { Continue }
| RETURN expr_opt SEMI                            { Return $2 }
;

expr_opt:
  expr { Some $1 }
|      { None }
;

/*
  表达式递归下降，优先级从低到高
*/
expr:
  expr OR expr1   { BinaryOp (Or, $1, $3) }
| expr1           { $1 }
;

expr1:
  expr1 AND expr2 { BinaryOp (And, $1, $3) }
| expr2           { $1 }
;

expr2:
  expr2 EQ expr3  { BinaryOp (Eq, $1, $3) }
| expr2 NEQ expr3 { BinaryOp (Neq, $1, $3) }
| expr2 LT expr3  { BinaryOp (Lt, $1, $3) }
| expr2 LEQ expr3 { BinaryOp (Leq, $1, $3) }
| expr2 GT expr3  { BinaryOp (Gt, $1, $3) }
| expr2 GEQ expr3 { BinaryOp (Geq, $1, $3) }
| expr3           { $1 }
;

expr3:
  expr3 PLUS expr4  { BinaryOp (Add, $1, $3) }
| expr3 MINUS expr4 { BinaryOp (Sub, $1, $3) }
| expr4             { $1 }
;

expr4:
  expr4 STAR expr5  { BinaryOp (Mul, $1, $3) }
| expr4 SLASH expr5 { BinaryOp (Div, $1, $3) }
| expr4 MOD expr5   { BinaryOp (Mod, $1, $3) }
| expr5             { $1 }
;

expr5:
  MINUS expr5 { UnaryOp (Neg, $2) }
| NOT expr5   { UnaryOp (Not, $2) }
| primary     { $1 }
;

primary:
  ID LPAREN args_opt RPAREN { Call ($1, $3) }
| ID                        { Var $1 }
| NUMBER                    { Int $1 }
| LPAREN expr RPAREN        { $2 }
;

args_opt:
  args { $1 }
|      { [] }
;

args:
  expr                { [$1] }
| args COMMA expr     { $1 @ [$3] }

