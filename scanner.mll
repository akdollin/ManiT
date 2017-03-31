(* Ocamllex scanner for ManiT *)

(*===---------------------------------------------------------------------===
 * Lexer
 * Currently assume that the only type is a double
 *===---------------------------------------------------------------------===*)

{ open Parser }

let string = '"' (([' '-'!' '#'-'[' ']'-'~'])* as s) '"'

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

(* types *)
(* type inference
| "int"    { INT }
| "bool"   { BOOL }
*)

(* keyword for func decl. see parser.*)
| "def"    { DEF }

(* Literals for each type. Order matters if same token matches two regexes
need regex for types: char, float, array,
*)
| "true"   { TRUE }
| "false"  { FALSE }
| string   { STRINGLIT(s) }
| ['0'-'9']+ as lxm { INTLIT(int_of_string lxm) }
| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID(lxm) }
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
  "*/" { token lexbuf }
| _    { comment lexbuf }
