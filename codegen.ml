(* codgen.ml
takes SAST and generates LLVM IR *)

module L = Llvm
module A = Sast
module S = Ast

module StringMap = Map.Make(String)

(* mutables *)
type mutables = {
  mutable prototypes : L.llvalue StringMap.t;
  mutable globals : L.llvalue StringMap.t;
};;

let translate (stmts) =
  let context = L.global_context () in
  let the_module = L.create_module context "ManiT"

  and i64_t  = L.i64_type  context
  and i32_t  = L.i32_type  context
  and i8_t   = L.i8_type   context
  and i1_t   = L.i1_type   context
  and void_t = L.void_type context in
  
  let ltype_of_typ = function
      A.Int -> i32_t
    | A.Float -> i64_t
    | A.Bool -> i1_t
    | A.Void -> void_t  (* need void? see return types *)  
    | _ -> i32_t (* due to error. add string *) in

  (* Declare printf(), which the print built-in function will call *)
  let printf_t = L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  let printf_func = L.declare_function "printf" printf_t the_module in
  
  (*
  (* main function *)
  let main_func = {
    A.fname = "main";
    A.typ = A.Int;
    A.formals = [];
    A.body = [];
    (* A.locals = []; *)
  } in

  let stmts = [main_func]@stmts in
  *)

  (* init globals *)
  let globals = 
    let rec build_global1 m e = match e with 
      A.Assign (id, right_e), t -> 
        let m = build_global1 m right_e in
        let init = L.const_int (ltype_of_typ t) 0 in
        StringMap.add id (L.define_global id init the_module) m 
      | _ -> m
    in
    let build_global2 m stmt = match stmt with 
        A.Expr e -> build_global1 m e
      | _ -> m
    in
  List.fold_left build_global2 StringMap.empty stmts in

  (* split fdecls and stmts. store stmts in main's body *)
  let (fdecls, main_body) = 
    let split (fdecls, main_body) stmt = match stmt with (* why func_decl_t?*) 
      A.Fdecl fdecl -> fdecls@[fdecl], main_body
    | _ -> fdecls, main_body@[stmt]
    and init = ([],[]) in
  List.fold_left split init stmts in 
  
  (* main_func *)
  let main_func = {
    A.fname = "main";
    A.typ = A.Int;
    A.formals = [];
    A.body = main_body
  } in

  (* functions *)
  let functions = [main_func]@fdecls in

  (* init prototypes *)
  let prototypes =  
    let build_proto1 m fdecl =
      let name = fdecl.A.fname
      and formal_types = 
      Array.of_list (List.map (fun (_, t) -> ltype_of_typ t) fdecl.A.formals) in 
        let ftype = L.function_type (ltype_of_typ fdecl.A.typ) formal_types in
    StringMap.add name (L.define_function name ftype the_module, fdecl) m in
  List.fold_left build_proto1 StringMap.empty functions in
  
  (* restructured
    let build_proto2 m stmt = match stmt with
      A.Fdecl fdecl -> build_proto1 m fdecl
      | _ -> m
    in
  List.fold_left build_proto2 StringMap.empty stmts in 
  *)


  (* Fill in the body of the given function *)
  let rec build_function fdecl =
    (* search prototype and get builder *)
    let (the_function, _ ) = StringMap.find fdecl.A.fname prototypes in
    let builder = L.builder_at_end context (L.entry_block the_function) in

    (* build format stringptr at top for print calls. see lec notes *)
    let int_format_str = L.build_global_stringptr "%s\n" "fmt" builder in

    (* formals *)
    let formals = 
      let add_formal m (id, t) param = L.set_value_name id param;
	(* allocate the formal and store param *)
        let formal = L.build_alloca (ltype_of_typ t) id builder in
      ignore (L.build_store param formal builder);
      StringMap.add id formal m in
    (* expr below evaluates to a map. see fold_left2 *)
    List.fold_left2 add_formal StringMap.empty fdecl.A.formals
      (Array.to_list (L.params the_function)) in

    (* add locals to formals *)
    let locals =
      let rec f1 m e = match e with
        A.Assign (id, right_e), t ->
          let m = f1 m right_e in
          let local_var  = L.build_alloca (ltype_of_typ t) id builder in
        StringMap.add id local_var m
        | _ -> m
      in
      let rec f2 m stmt = match stmt with
          A.Block sl -> f2 m stmt
        | A.Return e -> f1 m e
        | A.Expr e -> ( match fdecl.A.fname with 
            (* dont create locals for expr stmts in main b/c they are globals *)
            "main" -> m 
            | _    -> f1 m e )
        | A.If (p, t, e) -> List.fold_left f2 m [t; e]
        | A.While (p, b)-> f2 m stmt
        | A.For (e1, e2, e3, b) -> List.fold_left f1 m [e1; e2; e3]
        | _ -> m
      in
    List.fold_left f2 formals fdecl.A.body in

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
        A.IntLit i, t -> L.const_int i32_t i
      | A.FloatLit f, t -> L.const_float i64_t f 
      | A.BoolLit b, t -> L.const_int i1_t (if b then 1 else 0)
      | A.StringLit s, t -> L.build_global_stringptr s "" builder
      (* | A.Noexpr -> L.const_int i32_t 0 *)
      | A.Id s, t -> L.build_load (lookup s) s builder (* R.H.S lookup *)
      | A.Binop (e1, op, e2), t ->
          let e1' = build_expr builder e1
          and e2' = build_expr builder e2 in
          (match op with
            S.Add     -> L.build_add
          | S.Sub     -> L.build_sub
          | S.Mult    -> L.build_mul
          | S.Div     -> L.build_sdiv
          | S.And     -> L.build_and
          | S.Or      -> L.build_or
          | S.Equal   -> L.build_icmp L.Icmp.Eq
          | S.Neq     -> L.build_icmp L.Icmp.Ne
          | S.Less    -> L.build_icmp L.Icmp.Slt
          | S.Leq     -> L.build_icmp L.Icmp.Sle
          | S.Greater -> L.build_icmp L.Icmp.Sgt
          | S.Geq     -> L.build_icmp L.Icmp.Sge
          ) e1' e2' "tmp" builder
      | A.Unop(op, e), t ->
          let e' = build_expr builder e in
          (match op with
            S.Neg     -> L.build_neg
          | S.Not     -> L.build_not) e' "tmp" builder
      | A.Assign (id, e), t ->
        (* lookup id, store e` in s. stack alloced already. *)
        let e' = build_expr builder e in
          ignore (L.build_store e' (lookup id) builder); e'
      | A.Call ("print", [e]), t | A.Call ("printb", [e]), t ->
          L.build_call printf_func [| int_format_str ; (build_expr builder e) |]
            "printf" builder
      | A.Call (f, act), t ->
         let (fdef, fdecl) = StringMap.find f prototypes in
         let actuals = List.rev (List.map (build_expr builder) (List.rev act)) in
         let result = (match fdecl.A.typ with A.Void -> ""
                                            | _ -> f ^ "_result") in
         L.build_call fdef (Array.of_list actuals) result builder
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
        A.Block sl -> List.fold_left build_stmt builder sl
      | A.Expr e -> ignore (build_expr builder e); builder
      | A.Return e -> ignore (match fdecl.A.typ with
          A.Void -> L.build_ret_void builder
        | _ -> L.build_ret (build_expr builder e) builder); builder
      | A.If (predicate, then_stmt, else_stmt) ->
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

      | A.While (predicate, body) ->
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

      | A.For (e1, e2, e3, body) -> build_stmt builder
            ( A.Block [A.Expr e1 ; A.While (e2, A.Block [body ; A.Expr e3]) ] )
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
    let builder = build_stmt builder (A.Block fdecl.A.body) in
    
    (* Add a return if the last block falls off the end *)
    add_terminal builder (match fdecl.A.typ with
        A.Void -> L.build_ret_void
      | t -> L.build_ret (L.const_int (ltype_of_typ t) 0))
  in 

  List.iter build_function functions;
  the_module






(**************************** MicoC ****************************

module L = Llvm
module A = Ast

module StringMap = Map.Make(String)

let translate (stmts, functions) =
  let context = L.global_context () in
  let the_module = L.create_module context "MicroC"
  and i32_t  = L.i32_type  context
  and i8_t   = L.i8_type   context
  and i1_t   = L.i1_type   context
  and void_t = L.void_type context in

  let ltype_of_typ = function
      A.Int -> i32_t
    | A.Bool -> i1_t
    | A.Void -> void_t in

  let main_func = {
    A.fname = "main";
    A.typ = A.Int;
    A.formals = [];
    A.locals = [];
    A.body = pstmts;
  } in

  (* Declare each global variable; remember its value in a map *)
  let global_vars =
    let global_var m (t, n) =
      let init = L.const_int (ltype_of_typ t) 0
      in StringMap.add n (L.define_global n init the_module) m in
    List.fold_left global_var StringMap.empty globals in

  (* Declare printf(), which the print built-in function will call *)
  let printf_t = L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  let printf_func = L.declare_function "printf" printf_t the_module in

  (* Define each function (arguments and return type) so we can call it *)
  let functions = functions@[main_func] in

  let function_decls =
    let function_decl m fdecl =
      let name = fdecl.A.fname
      and formal_types =
	Array.of_list (List.map (fun (t,_) -> ltype_of_typ t) fdecl.A.formals)
      in let ftype = L.function_type (ltype_of_typ fdecl.A.typ) formal_types in
      StringMap.add name (L.define_function name ftype the_module, fdecl) m in
    List.fold_left function_decl StringMap.empty functions in
  
  (* Fill in the body of the given function *)
  let build_function_body fdecl =
    let (the_function, _) = StringMap.find fdecl.A.fname function_decls in
    let builder = L.builder_at_end context (L.entry_block the_function) in

    let int_format_str = L.build_global_stringptr "%s\n" "fmt" builder in
    
    (* Construct the function's "locals": formal arguments and locally
       declared variables.  Allocate each on the stack, initialize their
       value, if appropriate, and remember their values in the "locals" map
    let local_vars =
      let add_formal m (t, n) p = L.set_value_name n p;
	let local = L.build_alloca (ltype_of_typ t) n builder in
	ignore (L.build_store p local builder);
	StringMap.add n local m in

      let add_local m (t, n) =
	let local_var = L.build_alloca (ltype_of_typ t) n builder
	in StringMap.add n local_var m in

      let formals = List.fold_left2 add_formal StringMap.empty fdecl.A.formals
          (Array.to_list (L.params the_function)) in
      List.fold_left add_local formals fdecl.A.locals in

    (* Return the value for a variable or formal argument *)
    let lookup n = try StringMap.find n local_vars
                 with Not_found -> try StringMap.find n global_vars
                 with Not_found -> raise (Failure ("undeclared variable " ^ n))
    in

    (* Construct code for an expression; return its value *)
    let rec expr builder = function
	A.Literal i -> L.const_int i32_t i
      | A.BoolLit b -> L.const_int i1_t (if b then 1 else 0)
      | A.StringLit s -> L.build_global_stringptr s "" builder
      | A.Noexpr -> L.const_int i32_t 0
      | A.Id s -> L.build_load (lookup s) s builder
      | A.Binop (e1, op, e2) ->
	  let e1' = expr builder e1
	  and e2' = expr builder e2 in
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
      | A.Unop(op, e) ->
	  let e' = expr builder e in
	  (match op with
	    A.Neg     -> L.build_neg
          | A.Not     -> L.build_not) e' "tmp" builder
      | A.Assign (s, e) -> let e' = expr builder e in
	                   ignore (L.build_store e' (lookup s) builder); e'
      | A.Call ("print", [e]) | A.Call ("printb", [e]) ->
	  L.build_call printf_func [| int_format_str ; (expr builder e) |]
	    "printf" builder
      | A.Call (f, act) ->
         let (fdef, fdecl) = StringMap.find f function_decls in
	 let actuals = List.rev (List.map (expr builder) (List.rev act)) in
	 let result = (match fdecl.A.typ with A.Void -> ""
                                            | _ -> f ^ "_result") in
         L.build_call fdef (Array.of_list actuals) result builder
    in

    (* Invoke "f builder" if the current block doesn't already
       have a terminal (e.g., a branch). *)
    let add_terminal builder f =
      match L.block_terminator (L.insertion_block builder) with
	Some _ -> ()
      | None -> ignore (f builder) in
	
    (* Build the code for the given statement; return the builder for
       the statement's successor *)
    let rec stmt builder = function
	A.Block sl -> List.fold_left stmt builder sl
      | A.Expr e -> ignore (expr builder e); builder
      | A.Return e -> ignore (match fdecl.A.typ with
	  A.Void -> L.build_ret_void builder
	| _ -> L.build_ret (expr builder e) builder); builder
      | A.If (predicate, then_stmt, else_stmt) ->
         let bool_val = expr builder predicate in
	 let merge_bb = L.append_block context "merge" the_function in

	 let then_bb = L.append_block context "then" the_function in
	 add_terminal (stmt (L.builder_at_end context then_bb) then_stmt)
	   (L.build_br merge_bb);

	 let else_bb = L.append_block context "else" the_function in
	 add_terminal (stmt (L.builder_at_end context else_bb) else_stmt)
	   (L.build_br merge_bb);

	 ignore (L.build_cond_br bool_val then_bb else_bb builder);
	 L.builder_at_end context merge_bb

      | A.While (predicate, body) ->
	  let pred_bb = L.append_block context "while" the_function in
	  ignore (L.build_br pred_bb builder);

	  let body_bb = L.append_block context "while_body" the_function in
	  add_terminal (stmt (L.builder_at_end context body_bb) body)
	    (L.build_br pred_bb);

	  let pred_builder = L.builder_at_end context pred_bb in
	  let bool_val = expr pred_builder predicate in

	  let merge_bb = L.append_block context "merge" the_function in
          ******* MicroC ends here last few lines gone********)
*)



(******** Additional functions/types that can be reused *********)  
(* may use later. helper function to generate globals on first pass.
  (* can we use same func for locals? change define_global, module*) 
  (* function to generate globals from stmts *)
  let globals =
    let rec find_global m stmt_or_expr = 
    (
      match stmt_or_expr with
        A.Expr e -> find_global m e (* expr stmt *)
      | A.Assign (t, (id, e)) ->  (* assignment expr *)
          let init = L.const_int (ltype_of_typ t) 0 in
          let newMap = StringMap.add id (L.define_global id init the_module) m in
          find_global newMap e
      | _ -> m 
    ) in 
  List.fold_left find_global StringMap.empty stmts in 
*)


(* helper function to build fdecls on first pass. may use later.
  (* declare main_function *)
  let fdecls =
    (* map with main func. may be able to use list concat here *)
    let main_decl = List.fold_left build_fdecl StringMap.empty [main_func] in
    let find_fdecl m stmt = (
      match stmt with
        A.Fdecl -> build_fdecl m stmt (* have to declare Fdecl type? *)
      | _ -> m
    ) in
  List.fold_left find_fdecl main_decl stmts
*)
