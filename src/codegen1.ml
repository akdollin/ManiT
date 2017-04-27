(*
 * codegen.ml
 * takes SAST and generates LLVM IR
 *)

module L = Llvm
module S = Sast
module A = Ast
module StringMap = Map.Make(String)

(* mutables *)
type mutables = {
    mutable prototypes : L.llvalue StringMap.t;
    mutable globals : L.llvalue StringMap.t;
}

let translate (stmts) = 
    let context  = L.global_context () in
    let the_module = L.create_module context "ManiT"

    (* and i64_t = L.i64_type context *)
    and f32_t    = L.float_type  context
    and i32_t    = L.i32_type    context
    and i8_t     = L.i8_type     context
    and i1_t     = L.i1_type     context
    and void_t   = L.void_type   context
    and double_t = L.double_type context
    in
    let str_t    = L.pointer_type i8_t
    and ptr_t    = L.pointer_type i8_t
    in

    (*
    (* Hash table of the user defined structs *)
    let struct_types:(string, L.lltype) Hashtbl.t = Hashtbl.create 10
     *)
    (* 
     * Hash table of global variables. Maps a string, which is the name with
     * an L.llvalue.
     *)
    let global_variables:(string, L.llvalue) Hashtbl.t = Hashtbl.create 50 in
    
    (* helper function to find the type of a llvalue *)
    let type_of_llvalue v = L.type_of v in

    (*
     * Still needs implementation of structs and arrays and possibly doubles.
     * This is the new version of the previous ltype_of_typ
     *)
    let rec ltype_of_typ = function
        A.Int(s)    -> i32_t
      | A.Float(s)  -> double_t
      | A.String(s) -> string_t
      | A.Bool(s)   -> i1_t
      | _           -> void_t
    in

    (*
     * Llvm value of a literal expression
     *)
    let lvalue_of_lit = function
        A.IntLit    i -> L.const_int i32_t i
      | A.FloatLit  f -> L.const_float double_t f
      | A.BoolLit   b -> L.const_int i1_t b
      | A.StringLit s -> let l = L.define_global "unamed." (L.const_stringz context s) the_module in
                         L.const_bitcast (L.const_gep l [| L.const_int i32_t 0 |]) ptr_t
      | _             -> raise(Failure("Attempt to initialize global variable with non-const"))


    (*
     * Define default values for primitive typings. In this case, the string is
     * set to an empty string. Do not try and print this value, it will give
     * you should BS values since it is not null terminated, so it will flush
     * out whatever it is pointing to in memory. We set all the other values to
     * 0.
     *
     * Args:
         * t: A.typ type
     *)
    let default_value_for_type t = 
        match t with 
        | A.Int -> L.const_int (ltype_of_typ t) 0
        | A.Float -> L.const_float (ltype_of_typ t) 0.0
        | A.String -> L.const_string context "" 
        | A.Void -> L.const_int (ltype_of_typ t) 0
        | A.Bool -> L.const_int (ltype_of_typ t) 0
        | _ -> L.const_int ltype 777 (* for errors *) in 

    (*
     * Declaring each global varaible and storing value in a map.
     * It takes in a tuple: (t, n), where t is the type and n is the name of
     * the variable. We are simply defining these variables in the global
     * context here, so this function is only for the llvm to use. We won't
     * have access to the global_variables until we populate in the next
     * function. We only define the value with the basic 0 values.
     *
     * Args:
         * t: A.typ type
         * n: string name
     *)
    let define_global_with_value (t, n) ->
        match t with
        | A.Int     -> let init = L.const_int (ltype_of_typ t) 0 in (L.define_global n init the_module)
        | A.Float   -> let init = L.const_float (ltype_of_typ t) 0.0 in (L.define_global n init the_module)
        | A.String  -> let init = L.const_pointer_null (ltype_of_typ t) in (L.define_global n init the_module)
        | A.Void    -> let init = L.const_int (ltype_of_typ t) 0 in (L.define_global n init the_module)
        | A.Bool    -> let init = L.const_int (ltype_of_typ t) 0 in (L.define_global n init the_module) in

    (*
     * Now we add the global variables to the global data section. We add it to
     * the global_variables hash table that we made earlier.
     *
     * Args:
         * t: A.typ type
         * n: string name
     *)
    let define_global_var (t, n) = 
        match t with
        | A.Int(_) -> Hashtbl.add global_variables n (define_global_with_value (t,n))
        | A.Float(_) -> Hashtbl.add global_variables n (define_global_with_value (t,n))
        | A.String(_) -> Hashtbl.add global_variables n (define_global_with_value (t,n))
        | A.Void(_) -> Hashtbl.add global_variables n (define_global_with_value (t,n))
        | A.Bool(_) -> Hashtbl.add global_variables n (define_global_with_value (t,n)) in

    (*
     * n is the name, e is the expression on the right hand side.
     * Build the globals. Globals is a StringMap.
     *)
    let globals =
        let rec build_global n e =
            match e with
            | S.Assign (id, rhs), t -> (try StringMap.find id n; n with
                Not_found -> 
                    let init = default_value_for_type t in
                    StringMap.add id (L.define_global id init the_module) e) in
                    let n = build_global n e 
            | _ -> n
        in
        let build_global2 n stmt =
            match stmt with
            | S.Expr e -> build_global n e
            | _ -> n
        in
    List.fold_left build_global2 StringMap.empty stmts in

    (* Split the fdecls and stmts *)
