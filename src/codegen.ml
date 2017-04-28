(* codgen.ml
takes SAST and generates LLVM IR *)

module L = Llvm
module S = Sast
module A = Ast

module StringMap = Map.Make(String)

(* mutables. unused.
type mutables = {
  mutable prototypes : L.llvalue StringMap.t;
  mutable globals : L.llvalue StringMap.t;
};;
*)


let translate (stmts) =

  let context = L.global_context () in
  let the_module = L.create_module context "ManiT"

  (* and i64_t  = L.i64_type  context *)
  and f32_t  = L.double_type context
  and i32_t  = L.i32_type  context
  and i8_t   = L.i8_type   context
  and i1_t   = L.i1_type   context
  and void_t = L.void_type context in (* void? *)
  
  let string_t = L.pointer_type i8_t
  and i8ptr_t = L.pointer_type i8_t
  in
  
  (* Declare printf(), which the print built-in function will call *)
  let printf_t = L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  let printf_func = L.declare_function "printf" printf_t the_module in

  let _ltype_of_typ m = function
      A.Int -> i32_t
    | A.Float -> f32_t
    | A.String -> string_t (* ptr? *)
    | A.Bool -> i1_t
    | A.Void -> void_t  (* need void? see return types *)
    | A.Struct_typ(s) -> StringMap.find s m
    | _ -> i32_t (* placed due to error. add string *) in

  (* init value for each type
  let init_val = function
      A.Int -> 0
    | A.Float -> 0.0
    | A.Bool -> false
    | A.String -> "TEST: codegen init_val"
    | _ -> 777 (*for error checking *) in
  *)  
  
  (*
  let lvalue_of_lit e t =
      let ltype = ltype_of_typ t in
      match t with
        A.Int -> L.const_int ltype e
      | A.Float -> L.const_float ltype 0.0
      | A.Bool -> L.const_int ltype e
      | A.String -> L.const_pointer_null i8_t
      | _ -> L.const_int ltype 777 (* for errors *) in
    *)

  (* split fdecls and stmts. store stmts in main's body *)
  let (fdecls, sdecls, main_body) = 
    let split (fdecls, sdecls, main_body) stmt = match stmt with (* why func_decl_t?*) 
      S.Func(fdecl) -> fdecls@[fdecl], sdecls, main_body
    | S.Struc(sdecl) -> fdecls, sdecls@[sdecl], main_body
    | _ -> fdecls, sdecls, main_body@[stmt]
    and init = ([],[],[]) in
  List.fold_left split init stmts in 
  
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

  
  (* restructured code.
    let build_proto2 m stmt = match stmt with
      A.Fdecl fdecl -> build_proto1 m fdecl
      | _ -> m
    in
  List.fold_left build_proto2 StringMap.empty stmts in 
  *)

  let struct_hash:(string, L.lltype) Hashtbl.t = Hashtbl.create 10 in
  
  let make_struct strc =
    let struct_t = L.named_struct_type context strc.S.sname in
    Hashtbl.add struct_hash strc.S.sname struct_t in
    
  let struct_ltypes =
      let struct_ltype m st =
          let decls_array = Array.of_list ( List.rev ( List.fold_left 
              (fun l (t,_) -> (_ltype_of_typ m t)::l) [] st.S.vdecls) )
      in let named_struct = L.named_struct_type context st.S.sname
      in L.struct_set_body named_struct decls_array false ;
      StringMap.add st.S.sname named_struct m in
      List.fold_left struct_ltype StringMap.empty structs
  in
  
  let ltype_of_typ t = _ltype_of_typ struct_ltypes t in

  let make_struct_body strc =
    let struct_t = try Hashtbl.find struct_hash strc.S.sname with 
      | Not_found -> raise (Exceptions.BugCatch "struct not found in definition") in
    let vdecl_types = List.map (fun (typ, _) -> typ) strc.S.vdecls in
    let vdecls = List.map ltype_of_typ vdecl_types in
    let vdecl_array = Array.of_list vdecls in
    L.struct_set_body struct_t vdecl_array false in 
    
  let _ = List.iter make_struct structs in
  let _ = List.iter make_struct_body structs in
  
  let find_struct_name name =
    try Hashtbl.find struct_hash name
    with | Not_found -> raise(Exceptions.InvalidStruct name) in

  (* build prototypes *)
  let prototypes =  
    let build_proto1 m fdecl =
      let name = fdecl.S.fname
      and formal_types = 
      Array.of_list (List.map (fun (t, _) -> ltype_of_typ t) fdecl.S.formals) in 
        let ftype = L.function_type (ltype_of_typ fdecl.S.typ) formal_types in
    StringMap.add name (L.define_function name ftype the_module, fdecl) m in
  List.fold_left build_proto1 StringMap.empty functions in


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
      | S.Vdecl (typ, name) ->
        let init = L.const_named_struct (find_struct_name name) [||] in 
        (L.define_global name init the_module); m
      | _ -> m
    in
  List.fold_left build_global2 StringMap.empty stmts in

  
  (* Fill in the body of the given function *)
  let rec build_function fdecl =
    (* search prototype and get builder *)
    let (the_function, _ ) = StringMap.find fdecl.S.fname prototypes in
    let builder = L.builder_at_end context (L.entry_block the_function) in

    (* build format stringptr at top for print calls. see lec notes *)
    let int_format_str = L.build_global_stringptr "%d\n" "fmt" builder
      and float_format_str = L.build_global_stringptr "%f\n" "fmt" builder 
      and string_format_str = L.build_global_stringptr "%s\n" "fmt" builder in

    (* formals *)
    let formals = 
      let add_formal m (t, id) param = L.set_value_name id param;
  (* allocate the formal and store param *)
        let formal = L.build_alloca (ltype_of_typ t) id builder in
      ignore (L.build_store param formal builder);
      StringMap.add id formal m in
    (* expr below evaluates to a map. see fold_left2 *)
    List.fold_left2 add_formal StringMap.empty fdecl.S.formals
      (Array.to_list (L.params the_function)) in

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

    (*
    (* helper: allocates a new local (and global?) depending on fdecl. *)
    (* return value is a map but ignored in use *)
    let alloc_var m id t = match fdecl.A.fname with
        "main" ->
          let init = L.const_int (ltype_of_typ t) 0 in
          let global_var = L.define_global id init the_module in
          let globals = StringMap.add id global_var globals in
          globals
      | _ -> 
      let local_var = L.build_alloca (ltype_of_typ t) id builder in
    StringMap.add id local_var m in 
    *)

    (* original lookup: Return the value for a variable or formal argument *)
    let lookup id = try StringMap.find id locals
      with Not_found -> try StringMap.find id globals
      with Not_found -> raise (Failure ("undeclared variable " ^ id))
    in

    (*
    (* helper: searches for var and allocsif not found *)
    let lookup_alloc m id t = match fdecl.A.fname with
      "main" -> try StringMap.find id globals
          with Not_found -> alloc_var globals id t; lookup id
      | _ ->
      try StringMap.find id m (* how to match ""? compiler error *)
          with Not_found -> try StringMap.find id globals
          with Not_found -> alloc_var locals id t; lookup id
    in
    let f id m = try StringMap.find id m
      with Not_found -> let local_var = L.build_alloca (ltype_of_typ t) id builder in
    StringMap.add id local_var m
    *)

    (* Construct code for an expression; return its value *)
    let rec build_expr builder = function
        S.IntLit i, t -> L.const_int i32_t i
      | S.FloatLit f, t -> L.const_float f32_t f
      (*| A.DoubleLit d, t -> L.const_double i64_t d *)
      | S.BoolLit b, t -> L.const_int i1_t (if b then 1 else 0)
      | S.StringLit s, t -> L.build_global_stringptr s "" builder
      (* | A.Noexpr -> L.const_int i32_t 0 *)
      | S.Id s, t -> L.build_load (lookup s) s builder (* R.H.S lookup *)
      | S.Binop (e1, op, e2), t ->
          let e1' = build_expr builder e1
          and e2' = build_expr builder e2 in
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
          let e' = build_expr builder e in
          (match op with
            A.Neg     -> L.build_neg
          | A.Not     -> L.build_not) e' "tmp" builder
      | S.Assign (id, e), t ->
        (* lookup id, store e` in s. stack alloced already. *)
        let e' = build_expr builder e in
          ignore (L.build_store e' (lookup id) builder); e'
      | S.Call ("print", [(e, expr_t)]), t ->
      (*let t = (* find the type of it *) in*)
        let var = build_expr builder (e,expr_t) in
        (match expr_t with
        | A.Int ->
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
            L.build_call printf_func [| string_format_str ; (L.build_global_stringptr "" "" builder) |]) "printf" builder
      | S.Call (f, act), t ->
         let (fdef, fdecl) = StringMap.find f prototypes in
         let actuals = List.rev (List.map (build_expr builder) (List.rev act)) in
         let result = (match fdecl.S.typ with A.Void -> ""
                                            | _ -> f ^ "_result") in
         L.build_call fdef (Array.of_list actuals) result builder
      (*| S.Struct_make (s) -> L.build.malloc (find_struct_name s) "tmp" builder*)
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
    let rec build_stmt builder = function
      | S.Block sl -> List.fold_left build_stmt builder sl
      | S.Expr e -> ignore (build_expr builder e); builder
      | S.Return e -> ignore (match fdecl.S.typ with
          A.Void -> L.build_ret_void builder
        | _ -> L.build_ret (build_expr builder e) builder); builder
      | S.If (predicate, then_stmt, else_stmt) ->
         let bool_val = build_expr builder predicate in
         let merge_bb = L.append_block context "merge" the_function in

         let then_bb = L.append_block context "then" the_function in
         add_terminal (build_stmt (L.builder_at_end context then_bb) then_stmt)
           (L.build_br merge_bb);

         let else_bb = L.append_block context "else" the_function in
         add_terminal (build_stmt (L.builder_at_end context else_bb) else_stmt)
           (L.build_br merge_bb);

         ignore (L.build_cond_br bool_val then_bb else_bb builder);
         L.builder_at_end context merge_bb

      | S.While (predicate, body) ->
          let pred_bb = L.append_block context "while" the_function in
          ignore (L.build_br pred_bb builder);

          let body_bb = L.append_block context "while_body" the_function in
          add_terminal (build_stmt (L.builder_at_end context body_bb) body)
            (L.build_br pred_bb);

          let pred_builder = L.builder_at_end context pred_bb in
          let bool_val = build_expr pred_builder predicate in

          let merge_bb = L.append_block context "merge" the_function in
          ignore (L.build_cond_br bool_val body_bb merge_bb pred_builder);
          L.builder_at_end context merge_bb

      | S.For (e1, e2, e3, body) -> build_stmt builder
            ( S.Block [S.Expr e1 ; S.While (e2, S.Block [body ; S.Expr e3]) ] )

    in

    (* need to handle main separately to use this code.
    (* helper: if fdecl, recurse once and build fdecl. else, build stmt *)
    let build_all builder stmt = match stmt with (* need to pass builder? error due to List.iter *)
        A.Fdecl fdecl -> ignore(build_function fdecl) (* recursion *) 
      | _ -> ignore(build_stmt builder stmt)
    in
    (* Build the code for each stmt in the function body *)
    let _ = List.iter (fun _stmt -> build builder _stmt) fdecl.A.body in
    *)

    (* build code for each stmt in body. Cast to A.Block? *)
    let builder = build_stmt builder (S.Block fdecl.S.body) in
    
    (* Add a return if the last block falls off the end *)
    add_terminal builder (match fdecl.S.typ with
        A.Void -> L.build_ret_void
      | t -> L.build_ret (L.const_int (ltype_of_typ t) 0))
  in 

  List.iter build_function functions;
  the_module
