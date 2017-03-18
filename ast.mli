(* Abstract Syntax Tree and functions for printing it *)

type op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq |
          And | Or

type uop = Neg | Not

(* Table Literals *)
type key_literal =
    IntKey of int
  | StringKey of string

type literal = 
    IntLiteral of int
  | StringLiteral of string
  | FloatLiteral of float
  | Nolit

and table_literal =
    EmptyTable
  | ArrayLiteral of expr list
  | KeyValueLiteral of (key_literal * expr) list

type expr =
    Id of string
  | Binop of expr * op * expr
  | Literal of literal
  | TableLiteral of table_literal
  | Unop of uop * expr
  | Assign of string * expr
  | Call of string * expr list
  | TableAccess of string * (expr list)
  | ThisAccess of expr
  | TableAssign of string * (expr list) * expr

type stmt =
    Block of stmt list
  | Expr of expr
  | Func of func_decl
  | Return of expr
  | If of expr * stmt * stmt
  | For of expr * expr * expr * stmt
  | While of expr * stmt

type func_decl = {
    fname : string;
    formals: string list;
    body : stmt list;
  }

type program = stmt


