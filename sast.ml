(* semantically checked AST.
semant.ml takes AST and produces SAST while checking semantics 
*)
(*
type snum = 
    IntLit of int
*)
(*Not sure of ArrayLit and Array_access here. 
 * Other people have stuffs like
 * ArrayLit of expr_det list * datatype
 * Array_access of string * expr_det * datatype
 * No idea how we know datatype though.*)
type expr_det =
    IntLit of int
  | FloatLit of float
  | BoolLit of bool
  | StringLit of string
  | ArrayLit of Ast.allLits list
  | Id of string
  | Binop of expr_t * Ast.op * expr_t
  | Unop of Ast.uop * expr_t
  | Call of string * expr_t list
  | Array_access of string * expr_t
  | Assign of string * expr_t
  (* add array access here *)
  
  and expr_t = expr_det * Ast.typ (* typ comes first to match use in codegen *)

type stmt_t =
    Block of stmt_t list
  | Expr of expr_t
  | Return of expr_t
  | If of expr_t * stmt_t * stmt_t
  | For of expr_t * expr_t * expr_t * stmt_t
  | While of expr_t * stmt_t
  | Func of func_t 

  and func_t = {
    typ : Ast.typ; 
    fname : string;
    formals : (Ast.typ * string) list; 
    body : stmt_t list; (* need typed statements *)
   }

type symbol_table = {
  parent : symbol_table option;
  mutable variables: (string * Ast.typ) list
}

type environment = {
  scope: symbol_table;
  (* return: t option. can check manually. *)
}

type global_environment = {
  mutable funcs: func_t list;
  (* add globals here? design choice. global scope is the only scope w/o parent. *)
}

type program = stmt_t list
