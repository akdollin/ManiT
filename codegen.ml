(* Code generation: translate takes a semantically checked AST and
produces LLVM IR

LLVM tutorial: Make sure to read the OCaml version of the tutorial

http://llvm.org/docs/tutorial/index.html

Detailed documentation on the OCaml LLVM library:

http://llvm.moe/
http://llvm.moe/ocaml/

*)

module L = Llvm
module A = Ast

exception Error of string

(* module StringMap = Map.Make(String)

let translate (globals, functions) = *)

let context = L.global_context ()
let the_module = L.create_module context "ManiT"
let builder = builder context

let i32_t  = L.i32_type  context;;
let i8_t   = L.i8_type   context;;
let i1_t   = L.i1_type   context;;
let void_t = L.void_type context;;

(* Declare printf(), which the print built-in function will call *)
let print_t = L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
let print_func = L.declare_function "printf" print_t the_module in

(* Construct code for an expression; return its value *)
let rec expr llbuilder = function
  A.IntLiteral i  -> L.build_global_stringptr "Test" "" llbuilder
  |   A.BoolLiteral b   -> L.build_global_stringptr "Test" "" llbuilder
  |   A.FloatLiteral f  -> L.build_global_stringptr "Test" "" llbuilder
  |   A.StringLiteral s   -> L.build_global_stringptr s "" llbuilder
  |   A.Call(fname, el)   -> (function "print" -> 
    let print_t = L.var_arg_function_type i32_t [| pointer_type i8_t |] in
    let print = L.declare_function "print" print_t the_module in

    let s = expr llbuilder (List.hd el) in
    let zero = const_int i32_t 0 in
    let s = build_in_bounds_gep s [| zero |] "" llbuilder in
    build_call printf [| s |] "" llbuilder
