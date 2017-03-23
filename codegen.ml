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

module StringMap = Map.Make(String)

let translate (globals, functions) =
  let context = L.global_context () in
  let the_module = L.create_module context "ManiT"
  and i32_t  = L.i32_type  context
  and i8_t   = L.i8_type   context
  and i1_t   = L.i1_type   context
  and void_t = L.void_type context in
  
  (* Declare printf(), which the print built-in function will call *)
  let print_t = L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  let print_func = L.declare_function "printf" print_t the_module in

  (* Construct code for an expression; return its value *)
  let rec expr llbuilder = function
  A.Call ("print", [e]) ->
    L.build_call print_func [| (expr llbuilder e) |]
    "print" llbuilder
  in

  List.iter build_function_body functions;
  the_module
