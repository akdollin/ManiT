/* Ocamlyacc parser for ManiT */

%{
    open Ast;;
    let unescape s = Scanf.sscanf ("\"" ^ s ^ "\"") "%S%!" (fun x -> x)
%}

%token SEMI LPAREN RPAREN LBRACE RBRACE COMMA
%token LBRACK RBRACK
%token PLUS MINUS TIMES DIVIDE ASSIGN NOT
%token EQ NEQ LT LEQ GT GEQ TRUE FALSE AND OR
%token RETURN IF ELSE FOR WHILE
%token DEF GLOBAL STRUCT DOT

%token INT FLOAT BOOL STRING VOID
/* token VOID */

/* Literals */
%token <int> INTLIT
%token <float> FLOATLIT
%token <string> STRINGLIT
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
%left TIMES DIVIDE
%right NOT NEG
%right DOT

%start program
%type <Ast.program> program

%%

program:
  stmts EOF { List.rev $1 }

stmts:
    /* nothing */  { [] }
  | stmts stmt { $2 :: $1 }

stmt:
    expr SEMI { Expr $1 }
  | RETURN SEMI { Return Noexpr } 
  | RETURN expr SEMI { Return $2 }
  | LBRACE stmts RBRACE { Block(List.rev $2) }
  | IF LPAREN expr RPAREN stmt %prec NOELSE { If($3, $5, Block([])) }
  | IF LPAREN expr RPAREN stmt ELSE stmt    { If($3, $5, $7) }
  | FOR LPAREN expr_opt SEMI expr SEMI expr_opt RPAREN stmt 
     { For($3, $5, $7, $9) }
  | WHILE LPAREN expr RPAREN stmt { While($3, $5) }
  | DEF func { Func($2) }
  | STRUCT struct_dec { Struc($2) }


expr_opt: 
    /* nothing */ { Noexpr }
  | expr          { $1 }

struct_dec: 
  ID LBRACE expr_list RBRACE SEMI {{
    sname = $1; attributes = List.rev $3; }} 

func:
   typ ID LPAREN formals_opt RPAREN LBRACE stmts RBRACE
   { { typ = $1;
       fname = $2;
       formals = $4;
       body = List.rev $7 } }

typ:
    INT { Int }
  | BOOL { Bool }
  | FLOAT { Float }
  | STRING { String }
  | VOID   { Void }

formals_opt:
    /* nothing */ { [] }
  | formal_list   { List.rev $1 }

formal_list:
    typ ID { [($1, $2)] }
  | formal_list COMMA typ ID { ($3, $4)  :: $1 }

expr_list:
  { [] }
  | expr_list  proper_expr { $2::$1 }

proper_expr:
    expr SEMI { Expr $1 }

expr:
    INTLIT          { IntLit($1) }
  | FLOATLIT        { FloatLit($1) }
  | STRINGLIT   { StringLit(unescape $1) } 
  | TRUE             { BoolLit(true) }
  | FALSE            { BoolLit(false) }
  | ID               { Id($1) }
  | expr PLUS   expr { Binop($1, Add,   $3) }
  | expr MINUS  expr { Binop($1, Sub,   $3) }
  | expr TIMES  expr { Binop($1, Mult,  $3) }
  | expr DIVIDE expr { Binop($1, Div,   $3) }
  | expr EQ     expr { Binop($1, Equal, $3) }
  | expr NEQ    expr { Binop($1, Neq,   $3) }
  | expr LT     expr { Binop($1, Less,  $3) }
  | expr LEQ    expr { Binop($1, Leq,   $3) }
  | expr GT     expr { Binop($1, Greater, $3) }
  | expr GEQ    expr { Binop($1, Geq,   $3) }
  | expr AND    expr { Binop($1, And,   $3) }
  | expr OR     expr { Binop($1, Or,    $3) }
  | expr DOT    expr { Struct_access($1, $3)}
  | STRUCT ID        { Struct_create($2)}
  | MINUS expr %prec NEG { Unop(Neg, $2) } 
  | NOT expr         { Unop(Not, $2) }
  | ID ASSIGN expr   { Assign($1, $3) }
  | ID LPAREN actuals_opt RPAREN { Call($1, $3) }
  | LPAREN expr RPAREN { $2 }
  | GLOBAL ID ASSIGN expr { GlobalAsn($2, $4) } /* global asn */

actuals_opt:
    /* nothing */ { [] }
  | actuals_list  { List.rev $1 }

actuals_list:
    expr                    { [$1] }
  | actuals_list COMMA expr { $3 :: $1 }

