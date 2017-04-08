(* semantically checked AST.
semant.ml takes AST and produces SAST while checking semantics 
*)

(* need to add literal types:
float, array, etc 
is Void needed here? used in codegen *)
type t = Int | String | Bool | Float | Void

type expr_det =
    IntLit of int
  | FloatLit of float
  | BoolLit of bool
  | StringLit of string
  | Id of string
  | Binop of expr_t * Ast.op * expr_t
  | Unop of Ast.uop * expr_t
  | Call of string * expr_t list
  | Assign of string * expr_t
  (* add array access here *)
  and expr_t = expr_det * t (* typ comes first to match use in codegen *)

type stmt_t =
    Block of stmt_t list
  | Expr of expr_t
  | Return of expr_t
  | If of expr_t * stmt_t * stmt_t
  | For of expr_t * expr_t * expr_t * stmt_t
  | While of expr_t * stmt_t
  | Fdecl of func_decl_t (* need to attach type instead of placing in struct? *)

  and func_decl_t = {
    typ : t; (* ERROR: ok to introduce typ field? *)
    fname : string;
    formals : (string * t) list; (* where are typs? *)
    body : stmt_t list;
   }

type symbol_table = {
  parent : symbol_table option;
  mutable variables: (string * t) list
}

type environment = {
  scope: symbol_table;
  return: t option
}



type program = stmt_t list
