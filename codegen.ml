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
  and f32_t  = L.float_type context
  and i32_t  = L.i32_type  context
  and i8_t   = L.i8_type   context
  and i1_t   = L.i1_type   context
  and pointer_t = L.pointer_type
  and array_t = L.array_type
  and void_t = L.void_type context in (* void? *)
  
  (* Declare printf(), which the print built-in function will call *)
  let printf_t = L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  let printf_func = L.declare_function "printf" printf_t the_module in

  let rec  ltype_of_typ = function
      A.Int -> i32_t
    | A.Float -> f32_t
    | A.String -> pointer_t i8_t (* ptr? *)
    | A.Bool -> i1_t
    | A.Void -> void_t  (* need void? see return types *) 
    | A.Array(typ) ->
            (match typ with
                A.Int      ->  array_t i32_t  
                | A.Float  -> array_t f32_t
                | A.Bool -> array_t i1_t
                | A.String -> array_t i8_t
                | _ -> array_t void_t)
    (*| A.Array_access(t) ->
            (match t wit
                A.Int -> pointer_t i32_t
                | A.Float -> pointer_t float_t
                | _ -> pointer_t i32_t) *)
    | _ -> i32_t (* placed due to error. add string *) in

  (* init value for each type
  let init_val = function
      A.Int -> 0
    | A.Float -> 0.0
    | A.Bool -> false
    | A.String -> "TEST: codegen init_val"
    | _ -> 777 (*for error checking *) in
  *)  

(* Cause arrays... a repeat of the next one *)
let typeStart t = 
	match t with 
		  A.Int -> L.const_int i32_t 0
		| A.String ->L.const_string context "" 
(*		| A.Void ->L.const_int (ltype_of_typ t) 0 *)
		| A.Bool ->L.const_int (ltype_of_typ t) 0
in

 
  let rec init_llvalue id t =
    let ltype = ltype_of_typ t in
    match t with 
      A.Int | A.Bool -> L.const_int ltype 0
    | A.Float -> L.const_float ltype 0.0
    (*| A.String -> L.const_string context e *)
    | A.Array(typ, size) -> L.const_array (ltype_of_typ typ) (Array.make size (typeStart typ))

 (* L.const_array ( ltype_of_typ p) (array_of_zeros i (init_llvalue ((p)))) *)
   (* | A.Array -> L.cons *)
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
      | _ -> m
    in
  List.fold_left build_global2 StringMap.empty stmts in

  (* split fdecls and stmts. store stmts in main's body *)
  let (fdecls, main_body) = 
    let split (fdecls, main_body) stmt = match stmt with (* why func_decl_t?*) 
      S.Func fdecl -> fdecls@[fdecl], main_body
    | _ -> fdecls, main_body@[stmt]
    and init = ([],[]) in
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

  (* build prototypes *)
  let prototypes =  
    let build_proto1 m fdecl =
      let name = fdecl.S.fname
      and formal_types = 
      Array.of_list (List.map (fun (t, _) -> ltype_of_typ t) fdecl.S.formals) in 
        let ftype = L.function_type (ltype_of_typ fdecl.S.typ) formal_types in
    StringMap.add name (L.define_function name ftype the_module, fdecl) m in
  List.fold_left build_proto1 StringMap.empty functions in
  
  (* restructured code.
    let build_proto2 m stmt = match stmt with
      A.Fdecl fdecl -> build_proto1 m fdecl
      | _ -> m
    in
  List.fold_left build_proto2 StringMap.empty stmts in 
  *)


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
      | S.StringLit s, t -> L.build_global_string s "" builder
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
      | S.Call ("print", [e]), t | S.Call ("printb", [e]), t -> (* check the type, if float fomrat str, etc*)
          L.build_call printf_func [| int_format_str ; (build_expr builder e) |]
            "printf" builder
      | S.Call (f, act), t ->
         let (fdef, fdecl) = StringMap.find f prototypes in
         let actuals = List.rev (List.map (build_expr builder) (List.rev act)) in
         let result = (match fdecl.S.typ with A.Void -> ""
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
        S.Block sl -> List.fold_left build_stmt builder sl
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
