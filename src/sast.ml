(* semantically checked AST.
semant.ml takes AST and produces SAST while checking semantics 
*)

type bind = Ast.typ * string

type expr_det =
    IntLit of int
  | FloatLit of float
  | BoolLit of bool
  | StringLit of string
  | Id of string
  | Binop of expr_t * Ast.op * expr_t
  | Unop of Ast.uop * expr_t
  | Call of string * expr_t list
  | Assign of expr_t * expr_t
  | Array_create of expr_t list  (* Ast.typ holds length info *)
  | Array_access of string * int (* var, index *)
  | Struct_access of string * string * int (* var, attr, index, type *)
  
  and expr_t = expr_det * Ast.typ (* typ comes first to match use in codegen *)


type stmt_t =
    Block of stmt_t list
  | Expr of expr_t
  | Return of expr_t
  | If of expr_t * stmt_t * stmt_t
  | For of expr_t * expr_t * expr_t * stmt_t
  | While of expr_t * stmt_t
  | Func of func_t 
  | Struc of strc_t
  | Vdecl of bind

  and 
  func_t = {
    typ : Ast.typ; 
    fname : string;
    formals : (Ast.typ * string) list; 
    body : stmt_t list; (* need typed statements *)
  }

  and strc_t = {
    sname : string;
    vdecls : bind list;
  }

type symbol_table = {
  parent : symbol_table option;
  mutable variables: (string * Ast.typ) list
}

type environment = {
  scope: symbol_table;
}

type global_environment = {
  mutable funcs: func_t list;
}

type program = stmt_t list
