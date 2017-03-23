
module L = Llvm
module A = Ast

module StringMap = Map.Make(String)

let context = L.global_context () in
let the_module = L.create_module context "ManiT"
let i32_t  = L.i32_type  context;;
let i8_t   = L.i8_type   context;;
let i1_t   = L.i1_type   context;;
let void_t = L.void_type context;;

(* Declare printf(), which the print built-in function will call *)
let print_t = L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
let print_func = L.declare_function "printf" print_t the_module in

let builder = L.builder_at context in

(* Format string for print call *)
let string_format_str = unique_global_stringptr "%s" "sfmt." in

let rec expr ebuilder = function
    A.Call("print", [e]) ->
        L.build_call print_func [| string_format_str; (expr ebuilder e) |]
            "print" ebuilder
in
let rec stmt sbuilder = function
    A.Expr(e) -> ignore (expr sbuilder e); stmt sbuilder

in
let buildllvm s = stmt builder s
in
List.iter buildllvm 
