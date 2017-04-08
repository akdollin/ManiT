(* Abstract Syntax Tree.
contains ocaml types so that parser can generate these types from tokens *)

type op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq |
          And | Or

type uop = Neg | Not

(* type inference. Not sure about Void. See parser.
type typ = Int | Bool | Void
*)

(* these binds are no longer needed
type bind = typ * string
type bind_formals = string
type bind_vars = string * expr
*)

type expr =
    IntLit of int
  | BoolLit of bool
  | StringLit of string
  | Id of string
  | Binop of expr * op * expr
  | Unop of uop * expr
  | Assign of string * expr
  | Call of string * expr list (*fname and actuals*)
  | Noexpr (* ERROR? is Noexpr ok? *) 

type stmt =
    Block of stmt list
  | Expr of expr
  | Return of expr
  | If of expr * stmt * stmt
  | For of expr * expr * expr * stmt
  | While of expr * stmt

and 
type fdecl = {
    (* fdecl no longer has typ or locals. formals no longer have type. see parser.
    typ : typ; 
    formals : bind list;
    locals : bind list; *)
    fname : string;
    formals : string list;
    body : stmt list;
  }

type program = stmt list

(* created separate types to access from semant.ml 
type stmt_list = stmt list
type func_decl_list = func_decl list

type program = stmt_list * func_decl_list
(* MicroC. see parser. 
type program = bind list * func_decl list * stmt list
*) 

(* pretty print functions refactored to prettyprint.ml *)
*)
