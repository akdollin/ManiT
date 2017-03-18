/* Ocamlyacc parser for manit adopted from MicroC */

%{
open Ast
%}

%token LBRACK RBRACK
%token MOD
%token PERIOD CARROT
%token VAR STRUCT
%token <float> FLOAT

%token SEMI LPAREN RPAREN LBRACE RBRACE COMMA
%token PLUS MINUS TIMES DIVIDE ASSIGN NOT
%token EQ NEQ LT LEQ GT GEQ TRUE FALSE AND OR
%token RETURN IF ELSE FOR WHILE INT BOOL VOID
%token <int> LITERAL
%token <string> ID

%token EOF

%nonassoc NOELSE
%nonassoc ELSE
%right ASSIGN
%left OR
%left AND
%left EQ NEQ
%left LT GT LEQ GEQ
%left PLUS MINUS
%left TIMES DIVIDE MOD
%right NOT NEG

%start program
%type <Ast.program> program

%%

(* block on stmt_list*)
program:
  stmt_list EOF { $1 }
  
stmt_list:
    /* nothing */  { [] }
  | stmt stmt_list { $1 :: $2 }


formals_opt:
    /* nothing */ { [] }
  | formal_list   { List.rev $1 }

(* typ (type) *)
formal_list:
    ID                   { $1 }
  | formal_list COMMA ID { $4 :: $1 }

vdecl_list:
    /* nothing */    { [] }
  | vdecl_list vdecl { $2 :: $1 }

vdecl:
   typ ID SEMI { ($1, $2) }


(* Add in EMPTY statement *)
stmt:
    expr_no_bracket SEMI { Expr $1 }
  | SEMI { Empty }
  | RETURN expr SEMI { Return $2 }
  | LBRACE stmt_list RBRACE { Block($2) }
  | IF LPAREN expr RPAREN stmt %prec NOELSE { If($3, $5, Block([])) }
  | IF LPAREN expr RPAREN stmt ELSE stmt    { If($3, $5, $7) }
  | FOR LPAREN expr_opt SEMI expr SEMI expr_opt RPAREN stmt
     { For($3, $5, $7, $9) }
  | WHILE LPAREN expr RPAREN stmt { While($3, $5) }
  | VAR func_decl {Func($2)}

(* S/R error issue check *)
expr_opt:
    /* nothing */ { Noexpr }
  | expr_no_bracket         { $1 }

expr_no_bracket:
    LITERAL          { Literal($1) }
  | TRUE             { BoolLit(true) }
  | FALSE            { BoolLit(false) }
  | ID               { Id($1) }
  | expr_no_bracket PLUS   expr_no_bracket { Binop($1, Add,   $3) }
  | expr_no_bracket MINUS  expr_no_bracket { Binop($1, Sub,   $3) }
  | expr_no_bracket TIMES  expr_no_bracket { Binop($1, Mult,  $3) }
  | expr_no_bracket DIVIDE expr_no_bracket { Binop($1, Div,   $3) }
  | expr_no_bracket EQ     expr_no_bracket { Binop($1, Equal, $3) }
  | expr_no_bracket NEQ    expr_no_bracket { Binop($1, Neq,   $3) }
  | expr_no_bracket LT     expr_no_bracket { Binop($1, Less,  $3) }
  | expr_no_bracket LEQ    expr_no_bracket { Binop($1, Leq,   $3) }
  | expr_no_bracket GT     expr_no_bracket { Binop($1, Greater, $3) }
  | expr_no_bracket GEQ    expr_no_bracket { Binop($1, Geq,   $3) }
  | expr_no_bracket AND    expr_no_bracket { Binop($1, And,   $3) }
  | expr_no_bracket OR     expr_no_bracket { Binop($1, Or,    $3) }
  | MINUS expr %prec NEG { Unop(Neg, $2) }
  | NOT expr         { Unop(Not, $2) }
  | ID ASSIGN expr   { Assign($1, $3) }
  | ID LPAREN actuals_opt RPAREN { Call($1, $3) }
  | LPAREN expr RPAREN { $2 }

actuals_opt:
    /* nothing */ { [] }
  | actuals_list  { List.rev $1 }

actuals_list:
    expr                    { [$1] }
  | actuals_list COMMA expr { $3 :: $1 }
