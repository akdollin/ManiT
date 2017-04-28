(* Abstract Syntax Tree.
contains ocaml types so that parser can generate these types from tokens *)

type op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq |
          And | Or

type uop = Neg | Not

type typ = Int | Bool | Float | String | Void | Struct_typ of string

type bind = typ * string


(* need to check if formals have Void typ *)
(* type bind = typ * string
 *)

type expr =
    IntLit of int
  | FloatLit of float 
  | BoolLit of bool
  | StringLit of string
  | Id of string
  | Binop of expr * op * expr
  | Unop of uop * expr
  | Assign of string * expr
  | Call of string * expr list (*fname and actuals*)
  | Noexpr (* ERROR? is Noexpr ok? *) 
  | GlobalAsn of string * expr (* r.h.s can be local assingment. change codegen *)
  | Struct_make of string * string
  | Struct_access of expr * expr

(* type vdecl =
    Vdecl of typ * string
  (* | Assign of string * expr *) *)

type stmt =
    Block of stmt list
  | Expr of expr
  | Return of expr
  | If of expr * stmt * stmt
  | For of expr * expr * expr * stmt
  | While of expr * stmt
  | Func of func
  | Struc of strc

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
(*   vdecls : vdecl list;*)
}

type program = stmt list
