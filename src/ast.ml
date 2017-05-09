(* Abstract Syntax Tree.
contains ocaml types so that parser can generate these types from tokens *)

type op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq |
          And | Or

type uop = Neg | Not

type typ = Int | Bool | Float | String | Void | Struct_typ of string | Array_typ of typ * int

type bind = typ * string

(* need to check if formals have Void typ *)

type expr =
    IntLit of int
  | FloatLit of float 
  | BoolLit of bool
  | StringLit of string
  | Id of string
  | Binop of expr * op * expr
  | Unop of uop * expr
  | Assign of expr * expr
  | Call of string * expr list (*fname and actuals*)
  | Array_create of expr list
  | Array_access of expr * expr
  | Struct_access of expr * string (* can attr be an expr too? *)

type stmt =
    Block of stmt list
  | Expr of expr
  | Return of expr
  | If of expr * stmt * stmt
  | For of expr * expr * expr * stmt
  | While of expr * stmt
  | Func of func
  | Struc of strc
  | Vdecl of bind

and 
func = {
    typ : typ;
    fname : string;
    formals : (typ * string) list;
    body : stmt list;
}

and
strc = {
  sname : string;
  vdecls : bind list;
  fdecls : func list;
}

type program = stmt list
