type token =
  | LBRACK
  | RBRACK
  | MOD
  | PERIOD
  | CARROT
  | VAR
  | STRUCT
  | FLOAT of (float)
  | SEMI
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  | COMMA
  | PLUS
  | MINUS
  | TIMES
  | DIVIDE
  | ASSIGN
  | NOT
  | EQ
  | NEQ
  | LT
  | LEQ
  | GT
  | GEQ
  | TRUE
  | FALSE
  | AND
  | OR
  | RETURN
  | IF
  | ELSE
  | FOR
  | WHILE
  | INT
  | BOOL
  | VOID
  | LITERAL of (int)
  | ID of (string)
  | EOF

open Parsing;;
let _ = parse_error;;
# 4 "parser.mly"
open Ast
# 49 "parser.ml"
let yytransl_const = [|
  257 (* LBRACK *);
  258 (* RBRACK *);
  259 (* MOD *);
  260 (* PERIOD *);
  261 (* CARROT *);
  262 (* VAR *);
  263 (* STRUCT *);
  265 (* SEMI *);
  266 (* LPAREN *);
  267 (* RPAREN *);
  268 (* LBRACE *);
  269 (* RBRACE *);
  270 (* COMMA *);
  271 (* PLUS *);
  272 (* MINUS *);
  273 (* TIMES *);
  274 (* DIVIDE *);
  275 (* ASSIGN *);
  276 (* NOT *);
  277 (* EQ *);
  278 (* NEQ *);
  279 (* LT *);
  280 (* LEQ *);
  281 (* GT *);
  282 (* GEQ *);
  283 (* TRUE *);
  284 (* FALSE *);
  285 (* AND *);
  286 (* OR *);
  287 (* RETURN *);
  288 (* IF *);
  289 (* ELSE *);
  290 (* FOR *);
  291 (* WHILE *);
  292 (* INT *);
  293 (* BOOL *);
  294 (* VOID *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  264 (* FLOAT *);
  295 (* LITERAL *);
  296 (* ID *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\006\000\006\000\005\000\005\000\
\004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
\004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
\004\000\004\000\004\000\004\000\004\000\004\000\008\000\010\000\
\010\000\011\000\011\000\009\000\009\000\007\000\012\000\012\000\
\012\000\000\000"

let yylen = "\002\000\
\002\000\000\000\002\000\002\000\001\000\003\000\003\000\005\000\
\007\000\009\000\005\000\002\000\000\000\001\000\001\000\001\000\
\001\000\001\000\001\000\001\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\002\000\002\000\002\000\003\000\004\000\003\000\003\000\000\000\
\001\000\001\000\003\000\003\000\004\000\008\000\000\000\001\000\
\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\005\000\000\000\000\000\000\000\000\000\
\018\000\019\000\000\000\000\000\000\000\000\000\017\000\000\000\
\050\000\000\000\000\000\000\000\000\000\012\000\000\000\000\000\
\000\000\015\000\000\000\034\000\035\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\033\000\001\000\003\000\004\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\042\000\000\000\000\000\
\038\000\007\000\006\000\000\000\000\000\000\000\000\000\000\000\
\000\000\036\000\000\000\000\000\023\000\024\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\039\000\
\000\000\000\000\000\000\000\000\000\000\037\000\000\000\000\000\
\043\000\000\000\000\000\011\000\045\000\000\000\000\000\000\000\
\000\000\049\000\000\000\009\000\000\000\000\000\000\000\046\000\
\010\000"

let yydgoto = "\002\000\
\017\000\018\000\019\000\024\000\054\000\062\000\022\000\026\000\
\037\000\055\000\056\000\088\000"

let yysindex = "\022\000\
\094\255\000\000\020\255\000\000\051\255\094\255\249\254\249\254\
\000\000\000\000\051\255\019\255\025\255\028\255\000\000\009\255\
\000\000\030\000\094\255\100\000\018\255\000\000\051\255\116\000\
\048\255\000\000\047\255\000\000\000\000\053\255\051\255\249\254\
\051\255\051\255\051\255\051\255\000\000\000\000\000\000\000\000\
\249\254\249\254\249\254\249\254\249\254\249\254\249\254\249\254\
\249\254\249\254\249\254\249\254\060\255\000\000\061\255\059\255\
\000\000\000\000\000\000\065\255\116\000\071\255\076\255\080\255\
\077\255\000\000\250\254\250\254\000\000\000\000\159\000\159\000\
\100\255\100\255\100\255\100\255\147\000\132\000\049\255\000\000\
\051\255\094\255\051\255\094\255\093\255\000\000\085\255\090\255\
\000\000\072\255\098\255\000\000\000\000\049\255\096\255\094\255\
\249\254\000\000\094\255\000\000\101\255\106\255\094\255\000\000\
\000\000"

let yyrindex = "\000\000\
\109\000\000\000\000\000\000\000\000\000\107\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\121\255\
\000\000\000\000\002\000\000\000\000\000\000\000\109\255\013\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\104\255\
\000\000\000\000\113\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\014\255\
\000\000\000\000\000\000\000\000\044\255\000\000\000\000\000\000\
\000\000\000\000\171\255\196\255\000\000\000\000\227\255\084\000\
\221\255\028\000\053\000\078\000\063\255\055\255\116\255\000\000\
\000\000\000\000\000\000\000\000\146\255\000\000\120\255\000\000\
\000\000\001\000\000\000\000\000\000\000\116\255\000\000\000\000\
\129\255\000\000\107\255\000\000\000\000\000\000\000\000\000\000\
\000\000"

let yygindex = "\000\000\
\000\000\254\255\228\255\255\255\003\000\044\000\000\000\000\000\
\064\000\117\000\000\000\059\000"

let yytablesize = 441
let yytable = "\020\000\
\008\000\002\000\005\000\027\000\020\000\028\000\029\000\025\000\
\007\000\034\000\043\000\044\000\008\000\030\000\016\000\041\000\
\039\000\020\000\035\000\009\000\010\000\016\000\001\000\016\000\
\041\000\021\000\016\000\036\000\031\000\038\000\061\000\015\000\
\016\000\060\000\032\000\063\000\064\000\033\000\066\000\067\000\
\068\000\069\000\070\000\071\000\072\000\073\000\074\000\075\000\
\076\000\077\000\078\000\023\000\014\000\090\000\014\000\092\000\
\032\000\053\000\057\000\058\000\005\000\059\000\080\000\032\000\
\031\000\032\000\007\000\100\000\032\000\079\000\008\000\031\000\
\081\000\031\000\105\000\082\000\031\000\009\000\010\000\083\000\
\020\000\085\000\020\000\089\000\032\000\091\000\084\000\086\000\
\087\000\015\000\016\000\031\000\031\000\034\000\020\000\061\000\
\102\000\020\000\094\000\003\000\095\000\020\000\004\000\005\000\
\096\000\006\000\097\000\099\000\002\000\007\000\040\000\103\000\
\013\000\008\000\041\000\042\000\043\000\044\000\104\000\002\000\
\009\000\010\000\020\000\040\000\011\000\012\000\047\000\013\000\
\014\000\020\000\048\000\020\000\015\000\016\000\020\000\020\000\
\020\000\020\000\020\000\013\000\101\000\020\000\020\000\020\000\
\020\000\020\000\020\000\044\000\093\000\020\000\020\000\065\000\
\098\000\000\000\044\000\000\000\044\000\000\000\000\000\044\000\
\044\000\044\000\044\000\044\000\000\000\000\000\044\000\044\000\
\044\000\044\000\044\000\044\000\021\000\000\000\044\000\044\000\
\000\000\000\000\000\000\021\000\000\000\021\000\000\000\000\000\
\021\000\021\000\021\000\000\000\000\000\000\000\000\000\021\000\
\021\000\021\000\021\000\021\000\021\000\022\000\000\000\021\000\
\021\000\000\000\000\000\000\000\022\000\000\000\022\000\000\000\
\000\000\022\000\022\000\022\000\000\000\000\000\000\000\000\000\
\022\000\022\000\022\000\022\000\022\000\022\000\027\000\000\000\
\022\000\022\000\000\000\000\000\025\000\027\000\000\000\027\000\
\000\000\000\000\027\000\025\000\000\000\025\000\000\000\000\000\
\025\000\027\000\027\000\027\000\027\000\027\000\027\000\025\000\
\025\000\027\000\027\000\000\000\000\000\000\000\000\000\025\000\
\025\000\000\000\000\000\000\000\000\000\000\000\008\000\000\000\
\000\000\008\000\008\000\000\000\008\000\008\000\002\000\000\000\
\008\000\000\000\000\000\000\000\008\000\000\000\000\000\000\000\
\000\000\000\000\000\000\008\000\008\000\028\000\000\000\008\000\
\008\000\000\000\008\000\008\000\028\000\000\000\028\000\008\000\
\008\000\028\000\000\000\000\000\000\000\000\000\000\000\000\000\
\028\000\028\000\028\000\028\000\028\000\028\000\029\000\000\000\
\028\000\028\000\000\000\000\000\000\000\029\000\000\000\029\000\
\000\000\000\000\029\000\000\000\000\000\000\000\000\000\000\000\
\000\000\029\000\029\000\029\000\029\000\029\000\029\000\030\000\
\000\000\029\000\029\000\000\000\000\000\026\000\030\000\000\000\
\030\000\000\000\000\000\030\000\026\000\000\000\026\000\000\000\
\000\000\026\000\030\000\030\000\030\000\030\000\030\000\030\000\
\026\000\026\000\030\000\030\000\040\000\000\000\000\000\000\000\
\026\000\026\000\041\000\042\000\043\000\044\000\000\000\000\000\
\045\000\046\000\047\000\048\000\049\000\050\000\000\000\000\000\
\051\000\052\000\041\000\042\000\043\000\044\000\000\000\000\000\
\045\000\046\000\047\000\048\000\049\000\050\000\000\000\000\000\
\051\000\052\000\041\000\042\000\043\000\044\000\000\000\000\000\
\045\000\046\000\047\000\048\000\049\000\050\000\000\000\000\000\
\051\000\041\000\042\000\043\000\044\000\000\000\000\000\045\000\
\046\000\047\000\048\000\049\000\050\000\041\000\042\000\043\000\
\044\000\000\000\000\000\000\000\000\000\047\000\048\000\049\000\
\050\000"

let yycheck = "\001\000\
\000\000\000\000\010\001\006\000\006\000\007\000\008\000\005\000\
\016\001\001\001\017\001\018\001\020\001\011\000\002\001\002\001\
\019\000\019\000\010\001\027\001\028\001\009\001\001\000\011\001\
\011\001\006\001\014\001\019\001\010\001\000\000\032\000\039\001\
\040\001\031\000\010\001\033\000\034\000\010\001\036\000\041\000\
\042\000\043\000\044\000\045\000\046\000\047\000\048\000\049\000\
\050\000\051\000\052\000\001\001\009\001\082\000\011\001\084\000\
\002\001\040\001\011\001\013\001\010\001\009\001\002\001\009\001\
\002\001\011\001\016\001\096\000\014\001\010\001\020\001\009\001\
\014\001\011\001\103\000\011\001\014\001\027\001\028\001\009\001\
\082\000\002\001\084\000\081\000\030\001\083\000\011\001\011\001\
\040\001\039\001\040\001\029\001\030\001\001\001\096\000\097\000\
\099\000\099\000\014\001\006\001\011\001\103\000\009\001\010\001\
\033\001\012\001\009\001\012\001\000\000\016\001\002\001\011\001\
\009\001\020\001\015\001\016\001\017\001\018\001\013\001\013\001\
\027\001\028\001\002\001\011\001\031\001\032\001\011\001\034\001\
\035\001\009\001\011\001\011\001\039\001\040\001\014\001\015\001\
\016\001\017\001\018\001\011\001\097\000\021\001\022\001\023\001\
\024\001\025\001\026\001\002\001\085\000\029\001\030\001\035\000\
\094\000\255\255\009\001\255\255\011\001\255\255\255\255\014\001\
\015\001\016\001\017\001\018\001\255\255\255\255\021\001\022\001\
\023\001\024\001\025\001\026\001\002\001\255\255\029\001\030\001\
\255\255\255\255\255\255\009\001\255\255\011\001\255\255\255\255\
\014\001\015\001\016\001\255\255\255\255\255\255\255\255\021\001\
\022\001\023\001\024\001\025\001\026\001\002\001\255\255\029\001\
\030\001\255\255\255\255\255\255\009\001\255\255\011\001\255\255\
\255\255\014\001\015\001\016\001\255\255\255\255\255\255\255\255\
\021\001\022\001\023\001\024\001\025\001\026\001\002\001\255\255\
\029\001\030\001\255\255\255\255\002\001\009\001\255\255\011\001\
\255\255\255\255\014\001\009\001\255\255\011\001\255\255\255\255\
\014\001\021\001\022\001\023\001\024\001\025\001\026\001\021\001\
\022\001\029\001\030\001\255\255\255\255\255\255\255\255\029\001\
\030\001\255\255\255\255\255\255\255\255\255\255\006\001\255\255\
\255\255\009\001\010\001\255\255\012\001\013\001\013\001\255\255\
\016\001\255\255\255\255\255\255\020\001\255\255\255\255\255\255\
\255\255\255\255\255\255\027\001\028\001\002\001\255\255\031\001\
\032\001\255\255\034\001\035\001\009\001\255\255\011\001\039\001\
\040\001\014\001\255\255\255\255\255\255\255\255\255\255\255\255\
\021\001\022\001\023\001\024\001\025\001\026\001\002\001\255\255\
\029\001\030\001\255\255\255\255\255\255\009\001\255\255\011\001\
\255\255\255\255\014\001\255\255\255\255\255\255\255\255\255\255\
\255\255\021\001\022\001\023\001\024\001\025\001\026\001\002\001\
\255\255\029\001\030\001\255\255\255\255\002\001\009\001\255\255\
\011\001\255\255\255\255\014\001\009\001\255\255\011\001\255\255\
\255\255\014\001\021\001\022\001\023\001\024\001\025\001\026\001\
\021\001\022\001\029\001\030\001\009\001\255\255\255\255\255\255\
\029\001\030\001\015\001\016\001\017\001\018\001\255\255\255\255\
\021\001\022\001\023\001\024\001\025\001\026\001\255\255\255\255\
\029\001\030\001\015\001\016\001\017\001\018\001\255\255\255\255\
\021\001\022\001\023\001\024\001\025\001\026\001\255\255\255\255\
\029\001\030\001\015\001\016\001\017\001\018\001\255\255\255\255\
\021\001\022\001\023\001\024\001\025\001\026\001\255\255\255\255\
\029\001\015\001\016\001\017\001\018\001\255\255\255\255\021\001\
\022\001\023\001\024\001\025\001\026\001\015\001\016\001\017\001\
\018\001\255\255\255\255\255\255\255\255\023\001\024\001\025\001\
\026\001"

let yynames_const = "\
  LBRACK\000\
  RBRACK\000\
  MOD\000\
  PERIOD\000\
  CARROT\000\
  VAR\000\
  STRUCT\000\
  SEMI\000\
  LPAREN\000\
  RPAREN\000\
  LBRACE\000\
  RBRACE\000\
  COMMA\000\
  PLUS\000\
  MINUS\000\
  TIMES\000\
  DIVIDE\000\
  ASSIGN\000\
  NOT\000\
  EQ\000\
  NEQ\000\
  LT\000\
  LEQ\000\
  GT\000\
  GEQ\000\
  TRUE\000\
  FALSE\000\
  AND\000\
  OR\000\
  RETURN\000\
  IF\000\
  ELSE\000\
  FOR\000\
  WHILE\000\
  INT\000\
  BOOL\000\
  VOID\000\
  EOF\000\
  "

let yynames_block = "\
  FLOAT\000\
  LITERAL\000\
  ID\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 40 "parser.mly"
                ( _1 )
# 342 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    Obj.repr(
# 43 "parser.mly"
                   ( [] )
# 348 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt_list) in
    Obj.repr(
# 44 "parser.mly"
                   ( _1 :: _2 )
# 356 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr_no_bracket) in
    Obj.repr(
# 49 "parser.mly"
                         ( Expr _1 )
# 363 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 50 "parser.mly"
         ( Empty )
# 369 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 51 "parser.mly"
                     ( Return _2 )
# 376 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 52 "parser.mly"
                            ( Block(_2) )
# 383 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 53 "parser.mly"
                                            ( If(_3, _5, Block([])) )
# 391 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'stmt) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 54 "parser.mly"
                                            ( If(_3, _5, _7) )
# 400 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 6 : 'expr_opt) in
    let _5 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _7 = (Parsing.peek_val __caml_parser_env 2 : 'expr_opt) in
    let _9 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 56 "parser.mly"
     ( For(_3, _5, _7, _9) )
# 410 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 57 "parser.mly"
                                  ( While(_3, _5) )
# 418 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'func_decl) in
    Obj.repr(
# 58 "parser.mly"
                  (Func(_2))
# 425 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 62 "parser.mly"
                  ( Noexpr )
# 431 "parser.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr_no_bracket) in
    Obj.repr(
# 63 "parser.mly"
                            ( _1 )
# 438 "parser.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'array_literal) in
    Obj.repr(
# 66 "parser.mly"
                (ArrayLiteral(_1))
# 445 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr_no_bracket) in
    Obj.repr(
# 67 "parser.mly"
                    (_1)
# 452 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 70 "parser.mly"
                     ( Literal(_1) )
# 459 "parser.ml"
               : 'expr_no_bracket))
; (fun __caml_parser_env ->
    Obj.repr(
# 71 "parser.mly"
                     ( BoolLit(true) )
# 465 "parser.ml"
               : 'expr_no_bracket))
; (fun __caml_parser_env ->
    Obj.repr(
# 72 "parser.mly"
                     ( BoolLit(false) )
# 471 "parser.ml"
               : 'expr_no_bracket))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 73 "parser.mly"
                     ( Id(_1) )
# 478 "parser.ml"
               : 'expr_no_bracket))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr_no_bracket) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_no_bracket) in
    Obj.repr(
# 74 "parser.mly"
                                           ( Binop(_1, Add,   _3) )
# 486 "parser.ml"
               : 'expr_no_bracket))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr_no_bracket) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_no_bracket) in
    Obj.repr(
# 75 "parser.mly"
                                           ( Binop(_1, Sub,   _3) )
# 494 "parser.ml"
               : 'expr_no_bracket))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr_no_bracket) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_no_bracket) in
    Obj.repr(
# 76 "parser.mly"
                                           ( Binop(_1, Mult,  _3) )
# 502 "parser.ml"
               : 'expr_no_bracket))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr_no_bracket) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_no_bracket) in
    Obj.repr(
# 77 "parser.mly"
                                           ( Binop(_1, Div,   _3) )
# 510 "parser.ml"
               : 'expr_no_bracket))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr_no_bracket) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_no_bracket) in
    Obj.repr(
# 78 "parser.mly"
                                           ( Binop(_1, Equal, _3) )
# 518 "parser.ml"
               : 'expr_no_bracket))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr_no_bracket) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_no_bracket) in
    Obj.repr(
# 79 "parser.mly"
                                           ( Binop(_1, Neq,   _3) )
# 526 "parser.ml"
               : 'expr_no_bracket))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr_no_bracket) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_no_bracket) in
    Obj.repr(
# 80 "parser.mly"
                                           ( Binop(_1, Less,  _3) )
# 534 "parser.ml"
               : 'expr_no_bracket))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr_no_bracket) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_no_bracket) in
    Obj.repr(
# 81 "parser.mly"
                                           ( Binop(_1, Leq,   _3) )
# 542 "parser.ml"
               : 'expr_no_bracket))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr_no_bracket) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_no_bracket) in
    Obj.repr(
# 82 "parser.mly"
                                           ( Binop(_1, Greater, _3) )
# 550 "parser.ml"
               : 'expr_no_bracket))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr_no_bracket) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_no_bracket) in
    Obj.repr(
# 83 "parser.mly"
                                           ( Binop(_1, Geq,   _3) )
# 558 "parser.ml"
               : 'expr_no_bracket))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr_no_bracket) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_no_bracket) in
    Obj.repr(
# 84 "parser.mly"
                                           ( Binop(_1, And,   _3) )
# 566 "parser.ml"
               : 'expr_no_bracket))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr_no_bracket) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_no_bracket) in
    Obj.repr(
# 85 "parser.mly"
                                           ( Binop(_1, Or,    _3) )
# 574 "parser.ml"
               : 'expr_no_bracket))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'bracket_expr_list) in
    Obj.repr(
# 86 "parser.mly"
                         (TableAccess(_1,_2))
# 582 "parser.ml"
               : 'expr_no_bracket))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr_no_bracket) in
    Obj.repr(
# 87 "parser.mly"
                                    ( Unop(Neg, _2) )
# 589 "parser.ml"
               : 'expr_no_bracket))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr_no_bracket) in
    Obj.repr(
# 88 "parser.mly"
                                ( Unop(Not, _2) )
# 596 "parser.ml"
               : 'expr_no_bracket))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 89 "parser.mly"
                     ( Assign(_1, _3) )
# 604 "parser.ml"
               : 'expr_no_bracket))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'actuals_opt) in
    Obj.repr(
# 90 "parser.mly"
                                 ( Call(_1, _3) )
# 612 "parser.ml"
               : 'expr_no_bracket))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 91 "parser.mly"
                       ( _2 )
# 619 "parser.ml"
               : 'expr_no_bracket))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'actuals_opt) in
    Obj.repr(
# 94 "parser.mly"
                            (ArrayLiteral(_2))
# 626 "parser.ml"
               : 'array_literal))
; (fun __caml_parser_env ->
    Obj.repr(
# 97 "parser.mly"
                  ( [] )
# 632 "parser.ml"
               : 'actuals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'actuals_list) in
    Obj.repr(
# 98 "parser.mly"
                  ( List.rev _1 )
# 639 "parser.ml"
               : 'actuals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 101 "parser.mly"
                            ( [_1] )
# 646 "parser.ml"
               : 'actuals_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'actuals_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 102 "parser.mly"
                            ( _3 :: _1 )
# 654 "parser.ml"
               : 'actuals_list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 105 "parser.mly"
                       ([_2])
# 661 "parser.ml"
               : 'bracket_expr_list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'bracket_expr_list) in
    Obj.repr(
# 106 "parser.mly"
                                         ( _2 :: _4)
# 669 "parser.ml"
               : 'bracket_expr_list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : 'param_list) in
    let _7 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 109 "parser.mly"
                                                          ({fname=_2;params=_4;body=_7;})
# 678 "parser.ml"
               : 'func_decl))
; (fun __caml_parser_env ->
    Obj.repr(
# 112 "parser.mly"
  ([])
# 684 "parser.ml"
               : 'param_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 113 "parser.mly"
       ( [_1] )
# 691 "parser.ml"
               : 'param_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'param_list) in
    Obj.repr(
# 114 "parser.mly"
                        (_1::_3)
# 699 "parser.ml"
               : 'param_list))
(* Entry program *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let program (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Ast.program)
