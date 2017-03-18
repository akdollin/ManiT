(* Ocamllex scanner for MicroC *)

{
    open Parser
    open Util
}

(* standard character classes for numbers *)
let integer = ['0'-'9']+
let signed_int = ['+'  '-']? integer
let decimal = let decimal = ['+' '-']? (integer '.' ['0'-'9']* | '.' integer) (['e' 'E'] signed_int)?

rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf } (* Whitespace *)
  | "/*"     { comment lexbuf }           (* Comments *)
  | '('      { LPAREN }
  | ')'      { RPAREN }
  | '{'      { LBRACE }
  | '}'      { RBRACE }
  | ';'      { SEMI }
  | ','      { COMMA }
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
  | "+="     { PLUS_EQ }
  | "-="     { MINUS_EQ }
  | "*="     { TIMES_EQ }
  | "/="     { DIVIDE_EQ }
  | "&&"     { AND }
  | "||"     { OR }
  | "!"      { NOT }
  | "if"     { IF }
  | "else"   { ELSE }
  | "for"    { FOR }
  | "while"  { WHILE }
  | "return" { RETURN }
  | "true"   { TRUE }
  | "false"  { FALSE }
  | "var"    { VAR }
  | integer as lxm { LITERAL(int_of_string lxm) }
  | decimal as lxm { LITERAL(float_of_string lxm) }
  | ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID(lxm) }
  | eof { EOF }
  | _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

  (* Switch to this rule when a comment is encountered *)
  and comment = parse
    "*/" { token lexbuf }
    | _    { comment lexbuf }
