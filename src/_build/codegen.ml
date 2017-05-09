(* codgen.ml
takes SAST and generates LLVM IR *)

module L = Llvm
module S = Sast
module A = Ast
module StringMap = Map.Make(String)

let context = L.global_context ()
let the_module = L.create_module context "ManiT"

(* each is lltype *)
(* and i64_t  = L.i64_type  context *)
let d_t  = L.double_type context
let i64_t  = L.i64_type  context
let i32_t  = L.i32_type  context
let i8_t   = L.i8_type   context
let i1_t   = L.i1_type   context
let void_t = L.void_type context
let str_t = L.pointer_type i8_t
(* let i8ptr_t = L.pointer_type i8_t *)

(* struct types *)
let struct_types:(string, L.lltype) Hashtbl.t = Hashtbl.create 10

(* globals are initially empty *)
let globals:(string, L.llvalue) Hashtbl.t = Hashtbl.create 50

(* finds struct typ *)
let find_struct_typ name = try Hashtbl.find struct_types name
  with Not_found -> raise(Exceptions.InvalidStruct name)

(* Ast type to llvm type *)
let rec ltype_of_typ = function
    A.Int -> i32_t
  | A.Float -> d_t
  | A.String -> str_t 
  | A.Bool -> i1_t
  | A.Void -> void_t  (* need void? see return types *)
  | A.Struct_typ(sname) -> find_struct_typ sname (* assume that all struct typs all already made *)
  | A.Array_typ(elem_typ, length) -> L.array_type (ltype_of_typ elem_typ) length

(* Ast type to llvm value. *)
let rec lvalue_of_typ typ = match typ with
    A.Int | A.Bool | A. Void  -> L.const_int (ltype_of_typ typ) 0
  | A.Float -> L.const_float (ltype_of_typ typ) 0.0
  | A.String -> L.const_pointer_null  (ltype_of_typ typ)
  | A.Struct_typ(sname) -> L.const_named_struct (find_struct_typ sname) [||]
  | A.Array_typ(elem_typ, length) -> L.const_array (ltype_of_typ elem_typ) (Array.make length (lvalue_of_typ elem_typ))

(* expr to str
let expr_to_str e = match e with
  S.Id(str) -> str
  | _ -> raise (Exceptions.BugCatch "string_of_expr")
*)



(* declares struct typ *)
let declare_struct_typ s =
  let struct_t = L.named_struct_type context s.S.sname in
  Hashtbl.add struct_types s.S.sname struct_t

(* builds the body of struct typ *)
let define_struct_body s =
  let struct_typ = try Hashtbl.find struct_types s.S.sname 
    with Not_found -> raise(Exceptions.BugCatch "undefined define struct typ") in
  let vdecl_types = List.map (fun (typ, _) -> typ) s.S.vdecls in
  let vdecl_lltypes = Array.of_list (List.map ltype_of_typ vdecl_types) in
  L.struct_set_body struct_typ vdecl_lltypes false


(* ********* FUNCTIONS BEGIN HERE ********* *)
(* translates all functions to llvm IR *)
let translate_functions functions the_module =

(* Declare printf(), which the print built-in function will call *)
let printf_t = L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
let printf_func = L.declare_function "printf" printf_t the_module in

(* file open and close *)
(*
 *  * fopen takes 2 arguments, a filename, which is a string, and a mode (e.g. rw)
 *   * It returns a file pointer on success.
 *    *)
let open_file_t = L.function_type str_t [| str_t ; str_t |] in
let open_file_func = L.declare_function "fopen" open_file_t the_module in

let close_file_t = L.function_type i32_t [| str_t |] in
let close_file_func = L.declare_function "fclose" close_file_t the_module in

let fputs_t = L.function_type i32_t [| i32_t ; str_t |] in
let _ = L.declare_function "fputs" fputs_t the_module in

(*Args: str, num of chars to copy, file pointer*)
let fgets_t = L.function_type str_t [| str_t; i32_t; str_t |] in
let fgets_func = L.declare_function "fgets" fgets_t the_module in

let fwrite_t = L.function_type i32_t [| str_t; i32_t; i32_t; str_t |] in
let fwrite_func = L.declare_function "fwrite" fwrite_t the_module in

let fread_t = L.function_type i32_t [| str_t; i32_t; i32_t; str_t |] in
let fread_func = L.declare_function "fread" fread_t the_module in

let strlen_t = L.function_type i32_t [| str_t |] in
let strlen_func = L.declare_function "strlen" strlen_t the_module in

(* forking *)
let fork_t = L.function_type i32_t [||] in
let fork_func = L.declare_function "fork" fork_t the_module in

(* ******************* END FILE READ WRITE ******************** *)


(* build function prototypes *)
let prototypes =  
  let build_proto m fdecl =
    let name = fdecl.S.fname
    and formal_types = 
    Array.of_list (List.map (fun (t, _) -> ltype_of_typ t) fdecl.S.formals) in 
    let ftype = L.function_type (ltype_of_typ fdecl.S.typ) formal_types in
  StringMap.add name (L.define_function name ftype the_module, fdecl) m in
List.fold_left build_proto StringMap.empty functions in

(* format strings for printing. only in main_func *)
let (main_func, _) = try StringMap.find "main" prototypes
with Not_found -> raise(Exceptions.BugCatch "main function does not exist") in
let builder = L.builder_at_end context (L.entry_block main_func) in
let int_format_str = L.build_global_stringptr "%d\n" "fmt" builder
and float_format_str = L.build_global_stringptr "%f\n" "fmt" builder 
and string_format_str = L.build_global_stringptr "%s\n" "fmt" builder in

(* Core method that build llvm IR for fbody *)
let build_function fdecl =

  (* search prototype and get builder *)
  let (the_function, _ ) = StringMap.find fdecl.S.fname prototypes in
  let builder = L.builder_at_end context (L.entry_block the_function) in

  (* create formals. *)
  let formals = 
    let add_formal m (t, id) param = L.set_value_name id param;
    (* allocate the formal and store param *)
    let formal = L.build_alloca (ltype_of_typ t) id builder in
    ignore (L.build_store param formal builder);
    StringMap.add id formal m in
  (* expr below evaluates to a map. see fold_left2 *)
  List.fold_left2 add_formal StringMap.empty fdecl.S.formals
  (Array.to_list (L.params the_function)) in
  
  (* at start, formals are the only locals. added extra step to use hashtbl *)
  let locals:(string, L.llvalue) Hashtbl.t = Hashtbl.create 50 in
  let _ = StringMap.iter (fun id formal -> Hashtbl.add locals id formal) formals in

  (* original lookup: Return the value for a variable or formal argument *)
  (* no raise Failure here to use Not_found *)
  let find_var id = try Hashtbl.find locals id with Not_found -> Hashtbl.find globals id in
  
  (* print format typ. nested in Call? *)


 
  
  (* Allocates lhs when assignment is declaration *)
  let alloc_expr id typ in_block builder = 
    let init = match typ with
        A.Int | A.Float | A.Bool | A.String | A.Array_typ(_,_) -> lvalue_of_typ typ
      | _ -> raise(Failure("cannot alloc for exprs of these typs"))
    in
    
    (* if not in main and not in block, global. else, local *)
    (if ("main" = fdecl.S.fname) && not in_block 
    (* delcare and add to map *)
    then let global = L.define_global id init the_module in Hashtbl.add globals id global
    else let local =  L.build_alloca (ltype_of_typ typ) id builder in Hashtbl.add locals id local);
    builder in
  
  (* Allocates lhs in vdecl stmt *)
  let alloc_stmt id typ builder in_block =
    let init = match typ with
        A.Struct_typ(_) -> lvalue_of_typ typ
      | _ -> raise(Failure("cannot alloc for stmts of these typs")) in
    
    (* same as above *)
    (if ("main" = fdecl.S.fname) && not in_block 
    then let global = L.define_global id init the_module in Hashtbl.add globals id global
    else let local =  L.build_alloca (ltype_of_typ typ) id builder in Hashtbl.add locals id local);
    builder in

  (* Returns addr of expr. used for lhs of assignment expr *)
  let addr_of_expr expr typ in_b builder = match expr with
    S.Id(id) ->
      (* allocate space for lhs *)
      let var = try find_var id with Not_found ->
        (ignore(alloc_expr id typ in_b builder); find_var id) in var
    | S.Struct_access (var, _, index) ->
        let llvalue = find_var var in (* llvalue from build_alloca *)
        let addr = L.build_struct_gep llvalue index "tmp" builder in addr
    | S.Array_access(arrayName,index) ->
        let llvalue = try find_var arrayName with Not_found ->
          raise(Failure("Cannot assign to array that doesnt exist - Codegen")) in
        let addr = L.build_gep llvalue [| L.const_int i32_t 0; L.const_int i32_t index |] "array" builder in addr
    | _ -> raise(Failure("cannot get addr of LHS")) in

  (* Construct code for an expression; return its value *)
  let rec build_expr builder in_b = function
      S.IntLit i, _ -> L.const_int i32_t i
    | S.FloatLit f, _ -> L.const_float d_t f
    (*| A.DoubleLit d, t -> L.const_double i64_t d *)
    | S.BoolLit b, _ -> L.const_int i1_t (if b then 1 else 0)
    | S.StringLit s, _ -> L.build_global_stringptr s "" builder
    (* | A.Noexpr -> L.const_int i32_t 0 *)
    | S.Id s, _ -> L.build_load (find_var s) s builder (* R.H.S lookup *)
    | S.Binop (e1, op, e2), _ ->
        let e1' = build_expr builder in_b e1
        and e2' = build_expr builder in_b e2 in
        (match op with
          A.Add     -> L.build_add
        | A.Sub     -> L.build_sub
        | A.Mult    -> L.build_mul
        | A.Div     -> L.build_sdiv
        | A.And     -> L.build_and
        | A.Or      -> L.build_or
        | A.Equal   -> L.build_icmp L.Icmp.Eq
        | A.Neq     -> L.build_icmp L.Icmp.Ne
        | A.Less    -> L.build_icmp L.Icmp.Slt
        | A.Leq     -> L.build_icmp L.Icmp.Sle
        | A.Greater -> L.build_icmp L.Icmp.Sgt
        | A.Geq     -> L.build_icmp L.Icmp.Sge
        ) e1' e2' "tmp" builder
    | S.Unop(op, e), _ ->
        let e' = build_expr builder in_b e in
        (match op with
          A.Neg     -> L.build_neg
        | A.Not     -> L.build_not) e' "tmp" builder
    | S.Assign ((lhs, _), rhs), typ ->

       let l_val = (addr_of_expr lhs typ in_b builder)
(*        let target = match lhs with 
        S.Id(id) ->
          (* allocate space for lhs *)
          let var = try find_var id with Not_found ->
            (alloc_expr id typ in_b builder; find_var id) in var
        | S.Array_access(arrayName,index)-> raise(Failure("in array assign codegen"))
        | S.Struct_access (var, attr, index)-> let left = build_expr builder in_b (lhs, ltyp) in left 
        | _ -> raise(Failure("Id not found in codegen"))  *)
      in

      (* build rhs and store *)
      let e' = build_expr builder in_b rhs in
        ignore (L.build_store e' l_val builder); e' 
    | S.Call ("print", [(e, expr_t)]), _ ->
    (*let t = (* find the type of it *) in*)
      let var = build_expr builder in_b (e,expr_t) in
      (match expr_t with
        A.Int ->
          L.build_call printf_func [| int_format_str ; (var) |]
      | A.Float ->
          L.build_call printf_func [| float_format_str ; (var) |]
      | A.Bool ->
              (*
          let tr = L.build_global_stringptr "true" "" builder in
          let fa = L.build_global_stringptr "false" "" builder in
          if (L.is_null var) then L.build_call printf_func [| string_format_str ; (fa) |]
          else L.build_call printf_func [| string_format_str ; (tr) |] *)
          L.build_call printf_func [| int_format_str ; (var) |]
      | A.String ->
          L.build_call printf_func [| string_format_str ; (var) |]
      | A.Void ->
          L.build_call printf_func [| string_format_str ; (L.build_global_stringptr "" "" builder) |] 
      | _ -> raise(Failure("Call semant failed"))) "printf" builder
    (* built in functions *)
    | S.Call ("open", e), _ ->
        let actuals = List.rev (List.map (build_expr builder in_b) (List.rev e)) in
        L.build_call open_file_func (Array.of_list actuals) "fopen" builder
    | S.Call ("fgets", e), _ ->
        let actuals = List.rev (List.map (build_expr builder in_b) (List.rev e)) in
        L.build_call fgets_func (Array.of_list actuals) "fgets" builder
    | S.Call ("read", e), _ ->
        (*
        (* ptr *)
        let arg1 = List.nth 0 in
        (* size *)
        let arg2 = build_expr builder in_b (List.nth 1) in
        (* count *)
        let arg3 = build_expr builder in_b (List.nth 2) in
        (* file object *)
        let arg4 = build_expr builder in_b (List.nth 3) in
  *)
        let actuals = List.rev (List.map (build_expr builder in_b) (List.rev e)) in
        L.build_call fread_func (Array.of_list actuals) "fread" builder
    | S.Call ("write", e), _ ->
        let actuals = List.rev (List.map (build_expr builder in_b) (List.rev e)) in
        L.build_call fwrite_func (Array.of_list actuals) "fwrite" builder
    | S.Call ("len", e), _ ->
        let actuals = List.rev (List.map (build_expr builder in_b) (List.rev e)) in
        L.build_call strlen_func (Array.of_list actuals) "strlen" builder
    | S.Call ("close", e), _ ->
        let actuals = List.rev (List.map (build_expr builder in_b) (List.rev e)) in
        L.build_call close_file_func (Array.of_list actuals) "fclose" builder
    | S.Call ("fork", _), _ ->
        L.build_call fork_func (Array.of_list []) "fork" builder
    | S.Call (f, act), _ ->
       let (fdef, fdecl) = StringMap.find f prototypes in
       let actuals = List.rev (List.map (build_expr builder in_b) (List.rev act)) in
       let result = (match fdecl.S.typ with A.Void -> ""
                                          | _ -> f ^ "_result") in
       L.build_call fdef (Array.of_list actuals) result builder

    (* build array literal *)
    | S.Array_create (expr_list), arr_typ  ->
       (match arr_typ with 
          A.Array_typ(typ, length) ->
            let revElem_list = List.rev (expr_list) in
             let elems = Array.of_list (List.map (fun expr -> build_expr builder in_b expr) revElem_list) in
             let each_type = ltype_of_typ typ in
             let array_type = L.array_type each_type length in
             L.const_array array_type elems
        | _ -> raise(Failure("non-array types in create"))) 
    
    | S.Array_access (arr, index), _ ->
       let arr_lvalue = find_var arr in
       ignore(L.build_load arr_lvalue "loaded" builder); 
       let elem_ptr = L.build_gep arr_lvalue (*loaded_lvalue*) [|L.const_int i32_t 0; L.const_int i32_t index|] "arr addr" builder in
       L.build_load elem_ptr "array_access" builder

    | S.Struct_access (var, _ , index), _ ->
       let llvalue = find_var var in
       let addr = L.build_struct_gep llvalue index "tmp" builder in
       L.build_load addr "struct_access" builder
  in

  (* Invoke "f builder" if the current block doesn't already
     have a terminal (e.g., a branch). *)
  let add_terminal builder f =
    match L.block_terminator (L.insertion_block builder) with
      Some _ -> ()
    | None -> ignore (f builder) 
  in

  (* Build the code for the given statement; return the builder for
     the statement's successor *)
  let rec build_stmt builder in_b = function
    | S.Expr e -> ignore (build_expr builder in_b e); builder
    | S.Return e -> ignore (match fdecl.S.typ with
        A.Void -> L.build_ret_void builder
      | _ -> L.build_ret (build_expr builder in_b e) builder); builder

    (* if entering a block, need to keep track *)
    | S.Block sl -> List.fold_left (fun builder stmt -> build_stmt builder (*true*) in_b stmt) builder sl
    | S.If (predicate, then_stmt, else_stmt) ->
       let bool_val = build_expr builder true predicate in
       let merge_bb = L.append_block context "merge" the_function in

       let then_bb = L.append_block context "then" the_function in
       add_terminal (build_stmt (L.builder_at_end context then_bb) true then_stmt)
         (L.build_br merge_bb);

       let else_bb = L.append_block context "else" the_function in
       add_terminal (build_stmt (L.builder_at_end context else_bb) true else_stmt)
         (L.build_br merge_bb);

       ignore (L.build_cond_br bool_val then_bb else_bb builder);
       L.builder_at_end context merge_bb

    | S.While (predicate, body) ->
        let pred_bb = L.append_block context "while" the_function in
        ignore (L.build_br pred_bb builder);

        let body_bb = L.append_block context "while_body" the_function in
        add_terminal (build_stmt (L.builder_at_end context body_bb) true body)
          (L.build_br pred_bb);

        let pred_builder = L.builder_at_end context pred_bb in
        let bool_val = build_expr pred_builder true predicate in

        let merge_bb = L.append_block context "merge" the_function in
        ignore (L.build_cond_br bool_val body_bb merge_bb pred_builder);
        L.builder_at_end context merge_bb

    | S.For (e1, e2, e3, body) -> build_stmt builder true
          ( S.Block [S.Expr e1 ; S.While (e2, S.Block [body ; S.Expr e3]) ] )
    
    (* vdecl for structs. type checked in semant *)
    | S.Vdecl(typ, id) -> alloc_stmt id typ builder in_b
    | _ -> raise(Failure("Something went bad in codegen checkStatement"))
  in

  (* build code for each stmt in body. Cast to A.Block? *)
  let builder = build_stmt builder false (S.Block fdecl.S.body) in
  
  (* Add a return if the last block falls off the end *)
  add_terminal builder (match fdecl.S.typ with
      A.Void -> L.build_ret_void
    | t -> L.build_ret (L.const_int (ltype_of_typ t) 0))
in 

List.iter build_function functions;
the_module
(* ******* translate function ends here ****** *)


(* function to split fdecls and stmts. store stmts in main's body *)
let split stmts = 
  let split1 (fdecls, sdecls, main_body) stmt = match stmt with (* why func_decl_t?*) 
    S.Func(fdecl) -> fdecls@[fdecl], sdecls, main_body
  | S.Struc(sdecl) -> fdecls, sdecls@[sdecl], main_body
  | _ -> fdecls, sdecls, main_body@[stmt]
  and init = ([],[],[]) in
  List.fold_left split1 init stmts

(* tranlsate *)
let translate (stmts) =
  let (fdecls, sdecls, main_body) = split stmts in

  (* main_func *)
  let main_func = {
    S.fname = "main";
    S.typ = A.Int;
    S.formals = [];
    S.body = main_body
  } in
  
  (* functions *)
  let functions = [main_func]@fdecls in
  let structs = sdecls in

  let _ = List.iter declare_struct_typ structs in
  let _ = List.iter define_struct_body structs in
  let the_module = translate_functions functions the_module in
the_module


