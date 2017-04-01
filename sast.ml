(* semantically checked AST.
semant.ml takes AST and produces SAST while checking semantics 
*)

(* need to add literal types:
float, array, etc 
is Void needed here? used in codegen *)
type t = Int | String | Boolean | Void

type expr_det =
      IntLit of int
    | BoolLit of bool
    | StringLit of string
    | Id of string
    | Binop of expr_t * Ast.op * expr_t
    | Unop of Ast.uop * expr_t
    | Call of string * expr_t list
    | Call of string * expr_t list
    (* add array access here *)
    and expr_t = expr_det * t

type stmt_t =
      Block of stmt_t list
    | Expr of expr_t
    | Return of expr_t
    | If of expr_t * stmt_t * stmt_t
    | For of expr_t * expr_t * expr_t * stmt_t
    | While of expr_t * stmt_t

type func_decl_t = {
    fname : string;
    formals : string list;
    body : stmt_t list;
   }

type program = stmt_t list * func_decl_t list
