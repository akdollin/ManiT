/* Ocamlyacc parser for ManiT */

%{
    open Ast;;
    (* funcs no longer needed b/c program is a tuple. see CFG below. 
    let first (a,_,_) = a;;
    let second (_,b,_) = b;;
    let third (_,_,c) = c;;
    *)
    let unescape s = Scanf.sscanf ("\"" ^ s ^ "\"") "%S%!" (fun x -> x)
%}

%token SEMI LPAREN RPAREN LBRACE RBRACE COMMA
%token PLUS MINUS TIMES DIVIDE ASSIGN NOT
%token EQ NEQ LT LEQ GT GEQ TRUE FALSE AND OR
%token RETURN IF ELSE FOR WHILE
%token DEF (* function call *)
(* do we need VOID? where is it used? see RETURN; in CFG for stmt. not a scanned token in MicroC. 
%token VOID
*)

(* type inference.
%token INT BOOL
*)

(* Literals: 
new literal types in scanner should be added here.*) 
%token <int> INTLIT
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

%start program
%type <Ast.program> program

%%
(* CFG begins here.
- program is now a tuple of statements and functions.
- vdecl is removed.
- formals_opt, expr_opt, actuals_opt are distinct, and cannot be removed.
*)

program:
  decls EOF { $1 }

decls:
    /* nothing */ { [], [], [] }
  | decls stmt    { ($2 :: fst $1), snd $1 }
  | decls fdecl   { fst $1, ($2 :: snd $1) }

(* fdecl no longer have typ or locals (locals are in body) *)
fdecl:
   DEF ID LPAREN formals_opt RPAREN LBRACE stmt_list RBRACE
   { { fname = $2;
       formals = $4;
       body = List.rev $7 } }

formals_opt:
    /* nothing */ { [] }
  | formal_list   { List.rev $1 }

formal_list:
    ID { [$1] }
  | formal_list COMMA ID { $3 :: $1 }

stmt_list:
    /* nothing */  { [] }
  | stmt_list stmt { $2 :: $1 }

stmt:
    expr SEMI { Expr $1 } (* should "1+2;" be a valid program? *)
  | RETURN SEMI { Return Noexpr } (* ERROR? Can we return Noexpr for type inference? see semant*)
  | RETURN expr SEMI { Return $2 }
  | LBRACE stmt_list RBRACE { Block(List.rev $2) }
  | IF LPAREN expr RPAREN stmt %prec NOELSE { If($3, $5, Block([])) }
  | IF LPAREN expr RPAREN stmt ELSE stmt    { If($3, $5, $7) }
  | FOR LPAREN expr_opt SEMI expr SEMI expr_opt RPAREN stmt (* for(;;) is not allowed due to middle expr. *)
     { For($3, $5, $7, $9) }
  | WHILE LPAREN expr RPAREN stmt { While($3, $5) }

expr_opt: (* optional single expression. used in IF/FOR/WHILE *)
    /* nothing */ { Noexpr }
  | expr          { $1 }

expr:
  (* add literals here. *)
    NUMLIT          { Literal($1) }
  | STRING_LIT   { StringLit(unescape $1) } (* unescape is a function in header *)
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
  | MINUS expr %prec NEG { Unop(Neg, $2) } (* Brian does not understand this line *)
  | NOT expr         { Unop(Not, $2) }
  (* assignment replaces vdecl. see semant.ml *)
  | ID ASSIGN expr   { Assign($1, $3) }
  | ID LPAREN actuals_opt RPAREN { Call($1, $3) }
  | LPAREN expr RPAREN { $2 }

(* used for function call. *)
actuals_opt:
    /* nothing */ { [] }
  | actuals_list  { List.rev $1 }

actuals_list:
    expr                    { [$1] }
  | actuals_list COMMA expr { $3 :: $1 }


(* ****************************** MicroC *********************************

program:
  decls EOF { $1 }

(* should globals and stmts be together? *)
decls:
    /* nothing */ { [], [], [] }
 | decls vdecl { ($2 :: first $1), second $1, third $1 }
 | decls fdecl { first $1, ($2 :: second $1), third $1 }
 | decls stmt  { first $1, second $1, (third $1 @ [$2]) }

(* 
How do we infer function return type? maybe semant.ml
without keyword (fun or def), how do we diff btw func decl and func call?
added FUN keyword for now.
*)

(* formals, locals?*)
fdecl:
   DEF ID LPAREN formals_opt RPAREN LBRACE vdecl_list stmt_list RBRACE
   { { fname = $2;
       formals = $4;
       locals = List.rev $7;
       body = List.rev $8 } }

(* MicoC
fdecl:
   typ ID LPAREN formals_opt RPAREN LBRACE vdecl_list stmt_list RBRACE
     { { typ = $1;
	 fname = $2;
	 formals = $4;
	 locals = List.rev $7;
	 body = List.rev $8 } }
*)

formals_opt: (* optional formals list *)
    /* nothing */ { [] }
  | formal_list   { List.rev $1 }

(* HARD: how to infer types for formal args? possible b/c python does it. *)
formal_list:
    ID { [$1] }
  | formal_list COMMA ID { $3 :: $1 }

(* MicroC
formal_list:
    typ ID                   { [($1,$2)] }
  | formal_list COMMA typ ID { ($3,$4) :: $1 }
*)

(* MicroC. Again, VOID type needed?
typ:
    INT { Int }
  | BOOL { Bool }
  | VOID { Void }
*)

vdecl_list:
    /* nothing */    { [] }
  | vdecl_list vdecl { $2 :: $1 }

(* resolve var declaration and assignment.
1. can any expr be a rvalue with type inf?
2. do not delete assignment b/c var value can still change.
3. vdecl's type has changed. change AST, semant, codegen.
*)
(* same as assign op
*)
vdecl:
   ID ASSIGN expr SEMI { ($1, $3) }

(* type inf
vdecl:
   typ ID SEMI { ($1, $2) }
*)

stmt_list:
    /* nothing */  { [] }
  | stmt_list stmt { $2 :: $1 }

stmt:
    expr SEMI { Expr $1 } (* should "1+2;" be a valid program? *)
  | RETURN SEMI { Return Noexpr }
  | RETURN expr SEMI { Return $2 }
  | LBRACE stmt_list RBRACE { Block(List.rev $2) }
  | IF LPAREN expr RPAREN stmt %prec NOELSE { If($3, $5, Block([])) }
  | IF LPAREN expr RPAREN stmt ELSE stmt    { If($3, $5, $7) }
  | FOR LPAREN expr_opt SEMI expr SEMI expr_opt RPAREN stmt
     { For($3, $5, $7, $9) }
  | WHILE LPAREN expr RPAREN stmt { While($3, $5) }

expr_opt: (* optional expr list *)
    /* nothing */ { Noexpr }
  | expr          { $1 }

expr:
    LITERAL          { Literal($1) }
  | STRING_LITERAL   { StringLit(unescape $1) } (* unescape is func in header *)
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
  | MINUS expr %prec NEG { Unop(Neg, $2) } (* Brian does not understand this line *)
  | NOT expr         { Unop(Not, $2) }
  (*
  Next line might be a problem. Consider a = b = 1;
  b/c our var declaration is ID ASSIGN expr
  a will be a variable, but b might not be variable.
  however, we need this line to change the value of a variable.
  commented out for now.
  *)
  | ID ASSIGN expr   { Assign($1, $3) }
  | ID LPAREN actuals_opt RPAREN { Call($1, $3) }
  | LPAREN expr RPAREN { $2 }

(* when func is called, formals have to be matched with actuals. (in semant?)
at the same time, type of formals have to be inferred. *)
actuals_opt:
    /* nothing */ { [] }
  | actuals_list  { List.rev $1 }

actuals_list:
    expr                    { [$1] }
  | actuals_list COMMA expr { $3 :: $1 }


****************************** MicroC ********************************* *)
