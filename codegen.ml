
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

	let builder = L.builder_at_end context (L.entry_block statements) in

	(* Format string for print call *)
	let string_format_str = L.build_global_stringptr "%s\n" "sfmt" builder in

	let rec expr llbuilder = function
	    A.Call("print", [e]) ->
	        L.build_call print_func [| string_format_str; (expr llbuilder e) |]
	            "print" llbuilder
	in
	let rec stmt llbuilder = function
	    A.Expr(e) -> ignore (expr llbuilder e)

	in
	let buildllvm s = stmt builder s
	in
	List.iter buildllvm functions;
	the_module
