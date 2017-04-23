(* Ocamllex scanner for ManiT *)

(*===---------------------------------------------------------------------===
 * Lexer
 * Currently assume that the only type is a double
 *===---------------------------------------------------------------------===*)

{ open Parser }

let digits = ['0'-'9']+
let string = '"' (([' '-'!' '#'-'[' ']'-'~'])* as s) '"'
let float = ['+' '-']? (digits '.' ['0'-'9']* | '.' digits) (['e' 'E'] (['+' '-']? digits))?

rule token = parse
(* recursive call to eat white space *)
  [' ' '\t' '\r' '\n'] { token lexbuf } (* Whitespace *)
| "/*"     { comment lexbuf }           (* Comments *)

| '('      { LPAREN }
| ')'      { RPAREN }
| '{'      { LBRACE }
| '}'      { RBRACE }
| ';'      { SEMI }
| ','      { COMMA }
| '['      { LBRACK } 
| ']'      { RBRACK }

(* Operators *)
| '+'      { PLUS }
| '-'      { MINUS }
| '*'      { TIMES }
| '/'      { DIVIDE }
| '='      { ASSIGN }
| "=="     { EQ }
| "!="     { NEQ }
| '<'      { LT }
| "<="     { LEQ }
| ">"      { GT }
| ">="     { GEQ }
| "&&"     { AND }
| "||"     { OR }
| "!"      { NOT }

(* branch control *)
| "if"     { IF }
| "else"   { ELSE }
| "for"    { FOR }
| "while"  { WHILE }
| "return" { RETURN }

(* half-way type inf *)
| "int"    { INT }
| "bool"   { BOOL }
| "float"  { FLOAT }
| "string" { STRING }
| "void"   { VOID }

| "def"    { DEF }    (* keyword for func decl. see parser.*)
| "global" { GLOBAL } (* keyword for global assignment. see python *)
| "struct" { STRUCT } 

(* Literals for each type. 
Order matters if same token matches two regexes
need regex for types: char, float, array,
*)
| "true"   { TRUE }
| "false"  { FALSE }
| string   { STRINGLIT(s) }
| digits as lxm { INTLIT(int_of_string lxm) }
| float as lxm { FLOATLIT (float_of_string lxm) }
| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID(lxm) }
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
  "*/" { token lexbuf }
| _    { comment lexbuf }
