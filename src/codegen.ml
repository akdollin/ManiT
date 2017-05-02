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
  | _ -> i32_t (* placed due to error. *)

(* Ast type to llvm value. *)
let rec lvalue_of_typ typ = match typ with
    A.Int | A.Bool | A. Void  -> L.const_int (ltype_of_typ typ) 0
  | A.Float -> L.const_float (ltype_of_typ typ) 0.0
  | A.String -> L.const_pointer_null  (ltype_of_typ typ)
  | A.Struct_typ(sname) -> L.const_named_struct (find_struct_typ sname) [||]
  | A.Array_typ(elem_typ, length) -> L.const_array (ltype_of_typ elem_typ) (Array.make length (lvalue_of_typ elem_typ))
  | _ -> raise(Exceptions.BugCatch "no default value for this typ") 

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
let rec build_function fdecl =

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
  let find_var id = try Hashtbl.find locals id with Not_found -> Hashtbl.find globals id
  and find_local id = Hashtbl.find locals id 
  and find_global id = Hashtbl.find globals id in
  
  (* print format typ. nested in Call? *)

  (* unused
  (* Returns addr of expr. used for lhs of assignment expr *)
  let addr_of_expr expr builder = match expr with
      S.Id(id), _ -> find_var id
    | S.Struct_access(var, _ ,index), _ ->
        let llvalue = find_var var in (* llvalue from build_alloca *)
        let addr = L.build_struct_gep llvalue index "tmp" builder in addr
    | S.Array_access (arr_name, index), _ ->
        let llvalue = find_var arr_name in
        let addr = L.build_gep llvalue [| L.const_int i32_t 0; L.const_int i32_t index |] "array" builder in addr
    | _ -> raise(Failure("cannot get addr of LHS")) in
  *)
  
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
        
  (* Construct code for an expression; return its value *)
  let rec build_expr builder in_b = function
      S.IntLit i, t -> L.const_int i32_t i
    | S.FloatLit f, t -> L.const_float d_t f
    (*| A.DoubleLit d, t -> L.const_double i64_t d *)
    | S.BoolLit b, t -> L.const_int i1_t (if b then 1 else 0)
    | S.StringLit s, t -> L.build_global_stringptr s "" builder
    (* | A.Noexpr -> L.const_int i32_t 0 *)
    | S.Id s, t -> L.build_load (find_var s) s builder (* R.H.S lookup *)
    | S.Binop (e1, op, e2), t ->
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
    | S.Unop(op, e), t ->
        let e' = build_expr builder in_b e in
        (match op with
          A.Neg     -> L.build_neg
        | A.Not     -> L.build_not) e' "tmp" builder
    | S.Assign (id, e), typ ->
      (* allocate space for lhs *)
      let var = try find_var id with Not_found ->
        (alloc_expr id typ in_b builder; find_var id) in
      (* build rhs and store *)
      let e' = build_expr builder in_b e in
        ignore (L.build_store e' (var) builder); e'
    | S.Call ("print", [(e, expr_t)]), t ->
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
    | S.Call (f, act), t ->
       let (fdef, fdecl) = StringMap.find f prototypes in
       let actuals = List.rev (List.map (build_expr builder in_b) (List.rev act)) in
       let result = (match fdecl.S.typ with A.Void -> ""
                                          | _ -> f ^ "_result") in
       L.build_call fdef (Array.of_list actuals) result builder

    (* build array literal *)
    | S.Array_create (expr_list), arr_typ  ->
       (match arr_typ with 
          A.Array_typ(typ, length) ->
             let elems = Array.of_list (List.map (fun expr -> build_expr builder in_b expr) expr_list) in
             let each_type = ltype_of_typ typ in
             let array_type = L.array_type each_type length in
             L.const_array array_type elems
        | _ -> raise(Failure("non-array types in create"))) 
    
    | S.Array_access (arr, index), _ ->
       let arr_lvalue = find_var arr in
       let loaded_lvalue = L.build_load arr_lvalue "loaded" builder in
       let elem_ptr = L.build_gep arr_lvalue (*loaded_lvalue*) [|L.const_int i32_t 0; L.const_int i32_t index|] "arr addr" builder in
       L.build_load elem_ptr "array_access" builder

    | S.Struct_access (var, attr, index), t ->
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




(* unused stuff 

  (* struct type *)
  let make_struct strc =
    let struct_t = L.named_struct_type context strc.S.sname in
    Hashtbl.add struct_hash strc.S.sname struct_t in
  
  (* struct llvm types *)
  let struct_ltypes = (* is a map *)
    let struct_ltype m st =  (* allows recursive definition of structs *)
      (* array of vdecls. array needed for llvm func *)
      let decls_array = Array.of_list ( List.rev ( List.fold_left 
          (fun l (t,_) -> (_ltype_of_typ m t)::l) [] st.S.vdecls) ) in 
      let named_struct = L.named_struct_type context st.S.sname in 
      L.struct_set_body named_struct decls_array false;
      StringMap.add st.S.sname named_struct m in
  List.fold_left struct_ltype StringMap.empty structs
  in
  
  let ltype_of_typ t = _ltype_of_typ struct_ltypes t in

  let make_struct_body strc =
    let struct_t = try Hashtbl.find struct_hash strc.S.sname with 
      Not_found -> raise (Exceptions.BugCatch "struct not found in definition") in
    let vdecl_types = List.map (fun (typ, _) -> typ) strc.S.vdecls in
    let vdecls = List.map ltype_of_typ vdecl_types in
    let vdecl_array = Array.of_list vdecls in
    L.struct_set_body struct_t vdecl_array false in 
    
  let _ = List.iter make_struct structs in
  let _ = List.iter make_struct_body structs in
  
  let find_struct_name name =
    try Hashtbl.find struct_hash name
    with | Not_found -> raise(Exceptions.InvalidStruct name) in


  (* init values *)
  let init_llvalue id t =
    let ltype = ltype_of_typ t in
    match t with 
      A.Int | A.Bool -> L.const_int ltype 0
    | A.Float -> L.const_float ltype 0.0
    | A.String -> L.const_pointer_null string_t
    | _ -> L.const_int ltype 777 (* for errors *) in


  (* alloc globals *)
  let globals = 
    let rec build_global1 m e = match e with 
      (* alloc only if no global with same name was alloced previously *)
      S.Assign (id, right_e), t -> (try StringMap.find id m; m with
        Not_found -> 
        let m = build_global1 m right_e in
        (* factored out to add other types. see init_llvalue *)
        (* let init = L.const_float (ltype_of_typ t) (init_val t) in *)
        let init = init_llvalue id t in
        StringMap.add id (L.define_global id init the_module) m )
      (* skip other expr's *)
      | _ -> m
    in
    (* iterate on expr stmts, skip other stmts *)
    let build_global2 m stmt = match stmt with 
        S.Expr e -> build_global1 m e
      | S.Vdecl (typ, name) -> (match typ with 
          A.Struct_typ(typString) -> 
            let init = L.const_named_struct (find_struct_name typString) [||] in 
            (L.define_global name init the_module);m
        | _ -> raise(Failure("ManiT is type inferred. Something is wrong.")))
      | _ -> m
    in
  List.fold_left build_global2 StringMap.empty stmts in

  

    (* add locals to formals *)
    (* not in main => local *)
    (* in main but in block => local *)
    (* in main but not in block => global *)
    let locals =
      (* whether current scope is in a block. *)
      let rec build_local_e in_block map expr = 
        if in_block == true then match expr with
          S.Assign (id, right_e), typ ->
            let map =  build_local_e true map right_e in
            let local_var  = L.build_alloca (ltype_of_typ typ) id builder in
            StringMap.add id local_var map
          | _ -> map (* non asn stmt should not declare new vars *)
        else map (* if not in a block, global was created already. *)
      in
      (* add locals in stmts *)
      let rec build_local_s in_block map stmt = match stmt with
          S.Block stmt_list -> List.fold_left (fun map stmt -> build_local_s true map stmt) map stmt_list
        | S.If (pred, then_s, else_s) -> List.fold_left (fun map stmt -> build_local_s true map stmt) map [then_s; else_s]
        | S.While (pred, block) -> build_local_s true map block (* var declared in predicate ok? *)
        | S.For (e1, e2, e3, block) -> 
            let map = List.fold_left (fun map expr -> build_local_e true map expr) map [e1; e2; e3] in
            build_local_s true map block
        | S.Return expr -> build_local_e in_block map expr (* in_block follows that of parent *)
        | S.Expr expr -> build_local_e in_block map expr
        | _ -> map
      in
      let find_in_block = function
          "main" -> false
        | _ -> true
      in
      let in_block = find_in_block fdecl.S.fname in
      List.fold_left (fun map stmt -> build_local_s in_block map stmt) formals fdecl.S.body in
*)
