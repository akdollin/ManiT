(* Ocamllex scanner for ManiT *)

(*===---------------------------------------------------------------------===
 * Lexer
 * Currently assume that the only type is a double
 *===---------------------------------------------------------------------===*)

{
    open Parser
    let unescape s = Scanf.sscanf ("\"" ^ s ^ "\"") "%S%!" (fun x -> x)
}

let digit = ['0'-'9']
let digits = digit+
(* simpler versions to check error with struct access
let letter = ['a'-'z' 'A'-'Z'] 
let float = (digit+) ['.'] digit+
let string = '"' (letter (letter | digit)+ as s)'"'
*)

let ascii = ([' '-'!' '#'-'[' ']'-'~'])
let escape = '\\' ['\\' ''' '"' 'n' 'r' 't'] | '\\'
let string = '"' ((ascii | escape)* as s) '"' 

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
| "."      { DOT }


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
| string   { STRINGLIT(unescape s) }
| digits as lxm { INTLIT(int_of_string lxm) }
| float as lxm { FLOATLIT (float_of_string lxm) }
| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID(lxm) }
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
  "*/" { token lexbuf }
| _    { comment lexbuf }
