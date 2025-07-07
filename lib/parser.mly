%{
open Ast
%}

/* Tokens defined in lexer */
%token <string> ID
%token <int> NUMBER
%token INT VOID IF IFX ELSE WHILE BREAK CONTINUE RETURN
%token PLUS MINUS TIMES DIV MOD
%token EQ NEQ LE GE LT GT
%token LAND LOR NOT
%token ASSIGN
%token SEMI COMMA
%token LPAREN RPAREN
%token LBRACE RBRACE
%token EOF

%nonassoc IFX
%nonassoc ELSE (* 确保解析优先移入 ELSE *)
%left LOR
%left LAND
%nonassoc LT GT LE GE EQ NEQ
%left PLUS MINUS
%left TIMES DIV MOD
%right NOT UOP_MINUS UOP_PLUS

/* Start symbol */
%start comp_unit
%type <Ast.comp_unit> comp_unit

%%

comp_unit:
  | func_def_list EOF   { $1 }

func_def_list:
  | func_def                  { [$1] }
  | func_def_list func_def    { $1 @ [$2] }

func_def:
  | typ ID LPAREN param_list_opt RPAREN block
      {
        {
          ret_type = $1;
          func_name = $2;
          params = $4;
          body = $6;
        }
      }

typ:
  | INT   { TInt }
  | VOID  { TVoid }

param_list_opt:
  | /* empty */   { [] }
  | param_list    { $1 }

param_list:
  | param                   { [$1] }
  | param_list COMMA param  { $1 @ [$3] }

param:
  | INT ID   { $2 }  /* Only name is stored, type is always int */

block:
  | LBRACE stmt_list RBRACE   { $2 }

stmt_list:
  | /* empty */   { [] }
  | stmt_list stmt { $1 @ [$2] }

stmt:
  | block   { Block $1 }
  | SEMI    { Empty }
  | expr SEMI   { ExprStmt $1 }
  | ID ASSIGN expr SEMI   { Assign ($1, $3) }
  // 其实 toyc 里没有 79 行对应语法
  | INT ID SEMI   { Decl ($2, None) }  /* Declaration without initialization */
  | INT ID ASSIGN expr SEMI   { Decl ($2, Some $4) }
  | IF LPAREN expr RPAREN stmt %prec IFX
      { If ($3, $5, None) }
  | IF LPAREN expr RPAREN stmt ELSE stmt
      { If ($3, $5, Some $7) }
  | WHILE LPAREN expr RPAREN stmt
      { While ($3, $5) }
  | BREAK SEMI   { Break }
  | CONTINUE SEMI   { Continue }
  | RETURN SEMI   { Return None }
  | RETURN expr SEMI   { Return (Some $2) }

/* Expression hierarchy */
expr:
    | expr TIMES expr { Binop (Mul, $1, $3) }
    | expr DIV expr   { Binop (Div, $1, $3) }
    | expr MOD expr   { Binop (Mod, $1, $3)}
    | expr PLUS expr  { Binop (Add, $1, $3) }
    | expr MINUS expr { Binop (Sub, $1, $3) }
    | expr LE expr { Binop (Leq, $1, $3) }
    | expr GE expr { Binop (Geq, $1, $3) }
    | expr GT expr { Binop (Greater, $1, $3) }
    | expr LT expr { Binop (Less, $1, $3) }
    | expr EQ expr { Binop (Eq, $1, $3) }
    | expr NEQ expr { Binop (Neq, $1, $3) }
    | expr LAND expr { Binop (Land, $1, $3) }
    | expr LOR expr { Binop (Lor, $1, $3) }
    | NOT expr { Unop (Not, $2) }
    | MINUS expr { Unop (Minus, $2)} %prec UOP_MINUS
    | PLUS expr { Unop (Plus, $2)} %prec UOP_PLUS
    | primary { $1 }

primary:
  | ID   { ID $1 }
  | NUMBER   { Number $1 }
  | LPAREN expr RPAREN   { $2 }
  | ID LPAREN arg_list_opt RPAREN   { Call ($1, $3) }

arg_list_opt:
  | /* empty */   { [] }
  | arg_list      { $1 }

arg_list:
  | expr                { [$1] }
  | arg_list COMMA expr { $1 @ [$3] }

%%