type token =
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
  | VARIABLE
  | LITERAL of (int)
  | STRING_LITERAL of (string)
  | ID of (string)
  | EOF

open Parsing;;
let _ = parse_error;;
# 4 "parser.mly"
    open Ast;;
    let first (a,_,_) = a;;
    let second (_,b,_) = b;;
    let third (_,_,c) = c;;
    let unescape s = Scanf.sscanf ("\"" ^ s ^ "\"") "%S%!" (fun x -> x)
# 47 "parser.ml"
let yytransl_const = [|
  257 (* SEMI *);
  258 (* LPAREN *);
  259 (* RPAREN *);
  260 (* LBRACE *);
  261 (* RBRACE *);
  262 (* COMMA *);
  263 (* PLUS *);
  264 (* MINUS *);
  265 (* TIMES *);
  266 (* DIVIDE *);
  267 (* ASSIGN *);
  268 (* NOT *);
  269 (* EQ *);
  270 (* NEQ *);
  271 (* LT *);
  272 (* LEQ *);
  273 (* GT *);
  274 (* GEQ *);
  275 (* TRUE *);
  276 (* FALSE *);
  277 (* AND *);
  278 (* OR *);
  279 (* RETURN *);
  280 (* IF *);
  281 (* ELSE *);
  282 (* FOR *);
  283 (* WHILE *);
  284 (* INT *);
  285 (* BOOL *);
  286 (* VOID *);
  287 (* VARIABLE *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  288 (* LITERAL *);
  289 (* STRING_LITERAL *);
  290 (* ID *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\002\000\003\000\006\000\006\000\008\000\
\008\000\005\000\005\000\005\000\007\000\007\000\004\000\004\000\
\004\000\004\000\004\000\004\000\004\000\004\000\010\000\010\000\
\009\000\009\000\009\000\009\000\009\000\009\000\009\000\009\000\
\009\000\009\000\009\000\009\000\009\000\009\000\009\000\009\000\
\009\000\009\000\009\000\009\000\009\000\009\000\011\000\011\000\
\012\000\012\000\000\000"

let yylen = "\002\000\
\002\000\000\000\002\000\002\000\008\000\000\000\001\000\002\000\
\004\000\001\000\001\000\001\000\000\000\002\000\002\000\002\000\
\003\000\003\000\005\000\007\000\009\000\005\000\000\000\001\000\
\001\000\001\000\001\000\001\000\001\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\002\000\002\000\003\000\004\000\003\000\000\000\001\000\
\001\000\003\000\002\000"

let yydefred = "\000\000\
\002\000\000\000\051\000\000\000\000\000\013\000\000\000\000\000\
\027\000\028\000\000\000\000\000\000\000\000\000\010\000\011\000\
\012\000\025\000\026\000\000\000\001\000\003\000\004\000\000\000\
\000\000\000\000\000\000\042\000\043\000\016\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\015\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\046\000\018\000\014\000\017\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\032\000\033\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\045\000\000\000\
\000\000\000\000\000\000\000\000\000\000\022\000\000\000\008\000\
\000\000\000\000\000\000\000\000\013\000\000\000\020\000\000\000\
\000\000\009\000\000\000\005\000\021\000"

let yydgoto = "\002\000\
\003\000\004\000\022\000\053\000\024\000\082\000\027\000\083\000\
\025\000\057\000\060\000\061\000"

let yysindex = "\002\000\
\000\000\000\000\000\000\034\000\158\255\000\000\158\255\158\255\
\000\000\000\000\056\255\012\255\026\255\031\255\000\000\000\000\
\000\000\000\000\000\000\002\255\000\000\000\000\000\000\007\255\
\009\255\194\000\089\255\000\000\000\000\000\000\210\255\158\255\
\158\255\158\255\158\255\158\255\040\255\000\000\158\255\158\255\
\158\255\158\255\158\255\158\255\158\255\158\255\158\255\158\255\
\158\255\158\255\000\000\000\000\000\000\000\000\210\000\242\000\
\054\255\226\000\242\000\066\255\065\255\242\000\103\255\011\255\
\011\255\000\000\000\000\029\001\029\001\254\254\254\254\254\254\
\254\254\017\001\002\001\149\255\158\255\149\255\000\000\158\255\
\039\255\076\255\074\255\058\255\228\255\000\000\242\000\000\000\
\088\255\103\255\149\255\158\255\000\000\071\255\000\000\096\255\
\122\255\000\000\149\255\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\192\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\105\255\000\000\111\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\059\255\
\000\000\000\000\029\255\000\000\114\255\101\255\117\255\068\000\
\090\000\000\000\000\000\064\255\097\255\112\000\134\000\156\000\
\178\000\137\255\060\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\125\255\001\000\000\000\000\000\053\255\000\000\
\000\000\000\000\000\000\126\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\000\000\252\255\195\255\000\000\032\000\000\000\
\004\000\043\000\000\000\000\000"

let yytablesize = 559
let yytable = "\023\000\
\019\000\081\000\001\000\035\000\039\000\040\000\041\000\042\000\
\026\000\038\000\028\000\029\000\036\000\032\000\031\000\039\000\
\040\000\041\000\042\000\041\000\042\000\043\000\044\000\045\000\
\046\000\047\000\048\000\033\000\094\000\049\000\050\000\049\000\
\034\000\021\000\049\000\055\000\056\000\058\000\059\000\062\000\
\037\000\063\000\064\000\065\000\066\000\067\000\068\000\069\000\
\070\000\071\000\072\000\073\000\074\000\075\000\077\000\050\000\
\030\000\005\000\050\000\024\000\041\000\024\000\041\000\007\000\
\034\000\041\000\034\000\008\000\079\000\034\000\080\000\084\000\
\088\000\086\000\009\000\010\000\034\000\034\000\089\000\090\000\
\085\000\041\000\091\000\087\000\034\000\034\000\095\000\018\000\
\019\000\020\000\005\000\093\000\006\000\052\000\101\000\056\000\
\007\000\035\000\099\000\035\000\008\000\044\000\035\000\044\000\
\098\000\023\000\044\000\009\000\010\000\035\000\035\000\011\000\
\012\000\047\000\013\000\014\000\048\000\035\000\035\000\006\000\
\018\000\019\000\020\000\005\000\097\000\006\000\100\000\007\000\
\023\000\007\000\015\000\016\000\017\000\008\000\096\000\000\000\
\000\000\040\000\000\000\040\000\009\000\010\000\040\000\000\000\
\011\000\012\000\000\000\013\000\014\000\000\000\005\000\000\000\
\006\000\018\000\019\000\020\000\007\000\040\000\040\000\005\000\
\008\000\000\000\000\000\000\000\000\000\007\000\000\000\009\000\
\010\000\008\000\000\000\011\000\012\000\000\000\013\000\014\000\
\009\000\010\000\000\000\000\000\018\000\019\000\020\000\000\000\
\000\000\000\000\000\000\000\000\000\000\018\000\019\000\020\000\
\029\000\000\000\029\000\000\000\000\000\029\000\029\000\029\000\
\029\000\029\000\000\000\000\000\029\000\029\000\029\000\029\000\
\029\000\029\000\054\000\000\000\029\000\029\000\000\000\000\000\
\039\000\040\000\041\000\042\000\000\000\000\000\043\000\044\000\
\045\000\046\000\047\000\048\000\092\000\000\000\049\000\050\000\
\000\000\000\000\039\000\040\000\041\000\042\000\000\000\000\000\
\043\000\044\000\045\000\046\000\047\000\048\000\000\000\000\000\
\049\000\050\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\019\000\000\000\019\000\019\000\000\000\000\000\
\019\000\000\000\000\000\000\000\019\000\000\000\000\000\000\000\
\000\000\000\000\000\000\019\000\019\000\000\000\000\000\019\000\
\019\000\000\000\019\000\019\000\019\000\019\000\019\000\000\000\
\019\000\019\000\019\000\005\000\000\000\006\000\000\000\000\000\
\000\000\007\000\000\000\000\000\000\000\008\000\000\000\000\000\
\000\000\000\000\000\000\000\000\009\000\010\000\000\000\000\000\
\011\000\012\000\000\000\013\000\014\000\015\000\016\000\017\000\
\000\000\018\000\019\000\020\000\030\000\000\000\030\000\000\000\
\000\000\030\000\030\000\030\000\000\000\000\000\000\000\000\000\
\030\000\030\000\030\000\030\000\030\000\030\000\000\000\000\000\
\030\000\030\000\031\000\000\000\031\000\000\000\000\000\031\000\
\031\000\031\000\000\000\000\000\000\000\000\000\031\000\031\000\
\031\000\031\000\031\000\031\000\000\000\000\000\031\000\031\000\
\036\000\000\000\036\000\000\000\000\000\036\000\000\000\000\000\
\000\000\000\000\000\000\000\000\036\000\036\000\036\000\036\000\
\036\000\036\000\000\000\000\000\036\000\036\000\037\000\000\000\
\037\000\000\000\000\000\037\000\000\000\000\000\000\000\000\000\
\000\000\000\000\037\000\037\000\037\000\037\000\037\000\037\000\
\000\000\000\000\037\000\037\000\038\000\000\000\038\000\000\000\
\000\000\038\000\000\000\000\000\000\000\000\000\000\000\000\000\
\038\000\038\000\038\000\038\000\038\000\038\000\000\000\000\000\
\038\000\038\000\039\000\000\000\039\000\000\000\000\000\039\000\
\000\000\000\000\000\000\000\000\000\000\000\000\039\000\039\000\
\039\000\039\000\039\000\039\000\051\000\000\000\039\000\039\000\
\039\000\040\000\041\000\042\000\000\000\000\000\043\000\044\000\
\045\000\046\000\047\000\048\000\076\000\000\000\049\000\050\000\
\039\000\040\000\041\000\042\000\000\000\000\000\043\000\044\000\
\045\000\046\000\047\000\048\000\078\000\000\000\049\000\050\000\
\039\000\040\000\041\000\042\000\000\000\000\000\043\000\044\000\
\045\000\046\000\047\000\048\000\000\000\000\000\049\000\050\000\
\039\000\040\000\041\000\042\000\000\000\000\000\043\000\044\000\
\045\000\046\000\047\000\048\000\000\000\000\000\049\000\050\000\
\039\000\040\000\041\000\042\000\000\000\000\000\043\000\044\000\
\045\000\046\000\047\000\048\000\000\000\000\000\049\000\039\000\
\040\000\041\000\042\000\000\000\000\000\043\000\044\000\045\000\
\046\000\047\000\048\000\039\000\040\000\041\000\042\000\000\000\
\000\000\000\000\000\000\045\000\046\000\047\000\048\000"

let yycheck = "\004\000\
\000\000\063\000\001\000\002\001\007\001\008\001\009\001\010\001\
\005\000\001\001\007\000\008\000\011\001\002\001\011\000\007\001\
\008\001\009\001\010\001\009\001\010\001\013\001\014\001\015\001\
\016\001\017\001\018\001\002\001\090\000\021\001\022\001\003\001\
\002\001\000\000\006\001\032\000\033\000\034\000\035\000\036\000\
\034\001\002\001\039\000\040\000\041\000\042\000\043\000\044\000\
\045\000\046\000\047\000\048\000\049\000\050\000\001\001\003\001\
\001\001\002\001\006\001\001\001\001\001\003\001\003\001\008\001\
\001\001\006\001\003\001\012\001\003\001\006\001\006\001\076\000\
\034\001\078\000\019\001\020\001\013\001\014\001\003\001\006\001\
\077\000\022\001\025\001\080\000\021\001\022\001\091\000\032\001\
\033\001\034\001\002\001\004\001\004\001\005\001\099\000\092\000\
\008\001\001\001\003\001\003\001\012\001\001\001\006\001\003\001\
\034\001\001\001\006\001\019\001\020\001\013\001\014\001\023\001\
\024\001\003\001\026\001\027\001\003\001\021\001\022\001\003\001\
\032\001\033\001\034\001\002\001\093\000\004\001\005\001\003\001\
\003\001\008\001\028\001\029\001\030\001\012\001\092\000\255\255\
\255\255\001\001\255\255\003\001\019\001\020\001\006\001\255\255\
\023\001\024\001\255\255\026\001\027\001\255\255\002\001\255\255\
\004\001\032\001\033\001\034\001\008\001\021\001\022\001\002\001\
\012\001\255\255\255\255\255\255\255\255\008\001\255\255\019\001\
\020\001\012\001\255\255\023\001\024\001\255\255\026\001\027\001\
\019\001\020\001\255\255\255\255\032\001\033\001\034\001\255\255\
\255\255\255\255\255\255\255\255\255\255\032\001\033\001\034\001\
\001\001\255\255\003\001\255\255\255\255\006\001\007\001\008\001\
\009\001\010\001\255\255\255\255\013\001\014\001\015\001\016\001\
\017\001\018\001\001\001\255\255\021\001\022\001\255\255\255\255\
\007\001\008\001\009\001\010\001\255\255\255\255\013\001\014\001\
\015\001\016\001\017\001\018\001\001\001\255\255\021\001\022\001\
\255\255\255\255\007\001\008\001\009\001\010\001\255\255\255\255\
\013\001\014\001\015\001\016\001\017\001\018\001\255\255\255\255\
\021\001\022\001\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\002\001\255\255\004\001\005\001\255\255\255\255\
\008\001\255\255\255\255\255\255\012\001\255\255\255\255\255\255\
\255\255\255\255\255\255\019\001\020\001\255\255\255\255\023\001\
\024\001\255\255\026\001\027\001\028\001\029\001\030\001\255\255\
\032\001\033\001\034\001\002\001\255\255\004\001\255\255\255\255\
\255\255\008\001\255\255\255\255\255\255\012\001\255\255\255\255\
\255\255\255\255\255\255\255\255\019\001\020\001\255\255\255\255\
\023\001\024\001\255\255\026\001\027\001\028\001\029\001\030\001\
\255\255\032\001\033\001\034\001\001\001\255\255\003\001\255\255\
\255\255\006\001\007\001\008\001\255\255\255\255\255\255\255\255\
\013\001\014\001\015\001\016\001\017\001\018\001\255\255\255\255\
\021\001\022\001\001\001\255\255\003\001\255\255\255\255\006\001\
\007\001\008\001\255\255\255\255\255\255\255\255\013\001\014\001\
\015\001\016\001\017\001\018\001\255\255\255\255\021\001\022\001\
\001\001\255\255\003\001\255\255\255\255\006\001\255\255\255\255\
\255\255\255\255\255\255\255\255\013\001\014\001\015\001\016\001\
\017\001\018\001\255\255\255\255\021\001\022\001\001\001\255\255\
\003\001\255\255\255\255\006\001\255\255\255\255\255\255\255\255\
\255\255\255\255\013\001\014\001\015\001\016\001\017\001\018\001\
\255\255\255\255\021\001\022\001\001\001\255\255\003\001\255\255\
\255\255\006\001\255\255\255\255\255\255\255\255\255\255\255\255\
\013\001\014\001\015\001\016\001\017\001\018\001\255\255\255\255\
\021\001\022\001\001\001\255\255\003\001\255\255\255\255\006\001\
\255\255\255\255\255\255\255\255\255\255\255\255\013\001\014\001\
\015\001\016\001\017\001\018\001\003\001\255\255\021\001\022\001\
\007\001\008\001\009\001\010\001\255\255\255\255\013\001\014\001\
\015\001\016\001\017\001\018\001\003\001\255\255\021\001\022\001\
\007\001\008\001\009\001\010\001\255\255\255\255\013\001\014\001\
\015\001\016\001\017\001\018\001\003\001\255\255\021\001\022\001\
\007\001\008\001\009\001\010\001\255\255\255\255\013\001\014\001\
\015\001\016\001\017\001\018\001\255\255\255\255\021\001\022\001\
\007\001\008\001\009\001\010\001\255\255\255\255\013\001\014\001\
\015\001\016\001\017\001\018\001\255\255\255\255\021\001\022\001\
\007\001\008\001\009\001\010\001\255\255\255\255\013\001\014\001\
\015\001\016\001\017\001\018\001\255\255\255\255\021\001\007\001\
\008\001\009\001\010\001\255\255\255\255\013\001\014\001\015\001\
\016\001\017\001\018\001\007\001\008\001\009\001\010\001\255\255\
\255\255\255\255\255\255\015\001\016\001\017\001\018\001"

let yynames_const = "\
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
  VARIABLE\000\
  EOF\000\
  "

let yynames_block = "\
  LITERAL\000\
  STRING_LITERAL\000\
  ID\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decls) in
    Obj.repr(
# 37 "parser.mly"
            ( _1 )
# 353 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    Obj.repr(
# 40 "parser.mly"
                  ( [], [], [] )
# 359 "parser.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decls) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'fdecl) in
    Obj.repr(
# 41 "parser.mly"
               ( first _1, (_2 :: second _1), third _1 )
# 367 "parser.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decls) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 42 "parser.mly"
               ( first _1, second _1, (third _1 @ [_2]) )
# 375 "parser.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 7 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : 'formals_opt) in
    let _7 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 46 "parser.mly"
     ( { typ = _1;
	 fname = _2;
	 formals = _4;
	 body = List.rev _7 } )
# 388 "parser.ml"
               : 'fdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 52 "parser.mly"
                  ( [] )
# 394 "parser.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'formal_list) in
    Obj.repr(
# 53 "parser.mly"
                  ( List.rev _1 )
# 401 "parser.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 56 "parser.mly"
                             ( [(_1,_2)] )
# 409 "parser.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'formal_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'typ) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 57 "parser.mly"
                             ( (_3,_4) :: _1 )
# 418 "parser.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 60 "parser.mly"
      ( Int )
# 424 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 61 "parser.mly"
         ( Bool )
# 430 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 62 "parser.mly"
         ( Void )
# 436 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 65 "parser.mly"
                   ( [] )
# 442 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 66 "parser.mly"
                   ( _2 :: _1 )
# 450 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 69 "parser.mly"
              ( Expr _1 )
# 457 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 70 "parser.mly"
                ( Return Noexpr )
# 463 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 71 "parser.mly"
                     ( Return _2 )
# 470 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 72 "parser.mly"
                            ( Block(List.rev _2) )
# 477 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 73 "parser.mly"
                                            ( If(_3, _5, Block([])) )
# 485 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'stmt) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 74 "parser.mly"
                                            ( If(_3, _5, _7) )
# 494 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 6 : 'expr_opt) in
    let _5 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _7 = (Parsing.peek_val __caml_parser_env 2 : 'expr_opt) in
    let _9 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 76 "parser.mly"
     ( For(_3, _5, _7, _9) )
# 504 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 77 "parser.mly"
                                  ( While(_3, _5) )
# 512 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 80 "parser.mly"
                  ( Noexpr )
# 518 "parser.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 81 "parser.mly"
                  ( _1 )
# 525 "parser.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 84 "parser.mly"
                     ( Literal(_1) )
# 532 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 85 "parser.mly"
                     ( StringLit(unescape _1) )
# 539 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 86 "parser.mly"
                     ( BoolLit(true) )
# 545 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 87 "parser.mly"
                     ( BoolLit(false) )
# 551 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 88 "parser.mly"
                     ( Id(_1) )
# 558 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 89 "parser.mly"
                     ( Binop(_1, Add,   _3) )
# 566 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 90 "parser.mly"
                     ( Binop(_1, Sub,   _3) )
# 574 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 91 "parser.mly"
                     ( Binop(_1, Mult,  _3) )
# 582 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 92 "parser.mly"
                     ( Binop(_1, Div,   _3) )
# 590 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 93 "parser.mly"
                     ( Binop(_1, Equal, _3) )
# 598 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 94 "parser.mly"
                     ( Binop(_1, Neq,   _3) )
# 606 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 95 "parser.mly"
                     ( Binop(_1, Less,  _3) )
# 614 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 96 "parser.mly"
                     ( Binop(_1, Leq,   _3) )
# 622 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 97 "parser.mly"
                     ( Binop(_1, Greater, _3) )
# 630 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 98 "parser.mly"
                     ( Binop(_1, Geq,   _3) )
# 638 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 99 "parser.mly"
                     ( Binop(_1, And,   _3) )
# 646 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 100 "parser.mly"
                     ( Binop(_1, Or,    _3) )
# 654 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 101 "parser.mly"
                         ( Unop(Neg, _2) )
# 661 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 102 "parser.mly"
                     ( Unop(Not, _2) )
# 668 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 103 "parser.mly"
                     ( Assign(_1, _3) )
# 676 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'actuals_opt) in
    Obj.repr(
# 104 "parser.mly"
                                 ( Call(_1, _3) )
# 684 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 105 "parser.mly"
                       ( _2 )
# 691 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 108 "parser.mly"
                  ( [] )
# 697 "parser.ml"
               : 'actuals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'actuals_list) in
    Obj.repr(
# 109 "parser.mly"
                  ( List.rev _1 )
# 704 "parser.ml"
               : 'actuals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 112 "parser.mly"
                            ( [_1] )
# 711 "parser.ml"
               : 'actuals_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'actuals_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 113 "parser.mly"
                            ( _3 :: _1 )
# 719 "parser.ml"
               : 'actuals_list))
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