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
%left DOT LBRACK RBRACK

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
  | func { Func($1) }
  | STRUCT struct_decl { Struc($2) }
  | vdecl { Vdecl($1) }

expr_opt: 
    /* nothing */ { Noexpr }
  | expr          { $1 }

struct_typ:
  | STRUCT ID { $2 }

any_typ_not_void:
  | STRING  { String }
  | FLOAT   { Float }
  | INT     { Int }
  | BOOL    { Bool }
  | struct_typ    { Struct_typ($1) }

any_typ:
  | any_typ_not_void { $1 }
  | VOID  { Void } 

vdecl_list:
  { [] }
  | vdecl_list vdecl { $2 :: $1 }

vdecl:
    any_typ_not_void ID SEMI { ($1, $2) }

func:
   DEF any_typ ID LPAREN formals_opt RPAREN LBRACE stmts RBRACE
   { { typ = $2;
       fname = $3;
       formals = $5;
       body = List.rev $8 } }

struct_decl: 
  ID LBRACE vdecl_list struct_fdecls RBRACE SEMI 
  { { sname = $1; 
      vdecls = List.rev $3;
      fdecls  = List.rev $4 } }

struct_fdecls:
    /* nothing */ { [] }
  | func struct_fdecls { $1::$2 }

formals_opt:
    /* nothing */ { [] }
  | formal_list   { List.rev $1 }

formal_list:
    any_typ_not_void ID { [($1, $2)] }
  | formal_list COMMA any_typ_not_void ID { ($3, $4)  :: $1 }

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
  | MINUS expr %prec NEG { Unop(Neg, $2) } 
  | NOT expr         { Unop(Not, $2) }
  | LPAREN expr RPAREN { $2 }
  | expr ASSIGN expr   { Assign($1, $3) }
  | ID LPAREN exprs_opt RPAREN { Call($1, $3) }
  | GLOBAL ID ASSIGN expr { GlobalAsn($2, $4) } /* global asn */
  /* structs and arrays */
  | expr DOT ID { Struct_access($1, $3) }
  /* add struct fcall here */
  | LBRACK exprs_list RBRACK { Array_create($2) }
  | expr LBRACK expr RBRACK { Array_access($1,$3) }

exprs_opt:
    /* nothing */ { [] }
  | exprs_list  { List.rev $1 }

exprs_list:
    expr                    { [$1] }
  | exprs_list COMMA expr { $3 :: $1 }

