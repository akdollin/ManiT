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

(* conditionals *)
(* types *)
(* type inference 
| "int"    { INT }
| "bool"   { BOOL }
*)

(* keyword for func decl. see parser for why neccsary *)
| "def"    { DEF }

(* regardless of type inference, need different Literal for each type.
needed types: int (which is LITERAL in MicroC), char, string, float/double, boolean, array?...*)  
| "true"   { TRUE }
| "false"  { FALSE }
| string   { STRING_LITERAL(s) }
| ['0'-'9']+ as lxm { LITERAL(int_of_string lxm) }
| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID(lxm) }
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
  "*/" { token lexbuf }
| _    { comment lexbuf }
