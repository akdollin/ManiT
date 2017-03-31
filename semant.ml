(* Semantic checking for the MicroC compiler. 
checks semantics of AST and returns SAST. *)

(* current functionalities: 
- populates environment
*)

(* Note:
- in Ocaml func decl, (a : int) simply annotates that a is of type int. 
  useful for user declared types.
*)

open Sast

module StringMap = Map.Make(String)

(*find: helper function that finds given name in symbol table.
  in: symbol_table scope in environment, name
  out: vdecl *)
let rec find (scope : symbol_table) name = try
  List.find (fun (s, _) -> s = name) scope.variables with Not_found ->
  match scope.parent with
      Some(parent) -> find parent name
    | _ -> raise Not_found

let rec find_built_in name = try
  List.find (fun (s, _) -> s = name) built_in with Not_found -> raise Not_found
    
(*check_expr: core type-matching function that recursively annotates type of each expr.
  in : environment
  out : a type in SAST *)
let rec check_expr (env : environment) = function
  (* literals *)
  Ast.IntLit(l) -> IntLit(l), Int
  | Ast.StringLit(l) -> StringLit(l), String
  | Ast.BoolLit(l) -> BoolLit(l), Boolean
  (* ID(string)
  searches for scope with name v, the string of ID.
  find returns Not_found or vdecl, a tuple of value and type.
  use that type to annotate ID. *)
  | Ast.Id(v) -> let vdecl =
    try find env.scope v with Not_found -> 
    raise (Failure ("undeclared identifier " ^ v)) in
    let (v, typ) = vdecl in
    Id(v), typ
  (* Assignment(string, expr)
  checks expr of R.H.S, and compares type of expr to that of L.H.S from its declaration. 
  also, it populates scope's variable if typ does not match or not found. *)
  | Ast.Assign(v, e) ->
    let (e, typ) = check_expr env e in (* R.H.S typ *)
    let vdecl = try (* vdecl is set to decl *) 
      let decl = find env.scope v in
        if snd decl != typ then env.scope.variables <- 
          (decl :: env.scope.variables) ; decl
    with Not_found -> (*if find raises not found *)
      let decl = (v, typ) in env.scope.variables <-
        (decl :: env.scope.variables) ; decl
      in
      Assign(v, (e, typ)), typ
  (* Binop(expr, op, expr)
  checks types of L.H.S and R.H.S
  only allows ints (all OPs) for now.
  we need to specify rules for promotion/demotion of OPs *)
  | Ast.Binop (e1, op, e2) ->
    let e1 = check_expr env e1
    let e2 = check_expr env e2 in
  
    let _, t1 = e1
    let _, t2 = e2 in
  
    (
      match op with
      (*
      Ast.Plus -> 
      (
        match t1, t2 with
	  x, y when x = y && x != Table -> Binop(e1, op, e2), x
	| x, y when x = String || y = String -> Binop(e1, op, e2), String
	| Int, Double -> Binop(e1, op, e2), Double
	| Double, Int -> Binop(e1, op, e2), Double
	| _, _ -> raise(Failure("binary operation type mismatch"))
      )
      *)
      _ ->
      (
        match t1, t2 with
	Int, Int -> Binop(e1, op, e2), Int
	(* specify our rules here.
	Int, Int -> Binop(e1, op, e2), Int
	x, y when x = y && x != Table && x != String -> Binop(e1, op, e2), x
	| Int, Double -> Binop(e1, op, e2), Double
	| Double, Int -> Binop(e1, op, e2), Double
	*)
	| _ , _ -> raise(Failure("binary operation type mismatch or op not support these types"))
      )
    )
      
  (* Unop(uop, expr)
  uop is either Neg or Not *)
  | Ast.Unop(uop, e) ->
      let (e, typ) = check_expr env e in
      (
	(* this code can be cleaned up b/c value is same regardless of typ *)
	match uop with
	  Neg -> 
	  if typ != Int && typ != Double 
	  then raise (Failure("unary minus opeartion does not support this type ")) ;
	  Unop(uop, (e, typ)), typ
	| Not ->
	  if typ != Bool
	  then raise (Failure("unary not operation does not support this type "));
	  Unop(uop, (e, typ)), typ
      )
  (* ERROR function call: *)
  | Ast.Call(v, e) ->
  (*  Walk through func body and infer *)
  Id("dummy"), Int
  (* Need table access here. *)
      
      



(* check_stmt :
 input : env
 output : annotated stmt in SAST.
 *)
let rec check_stmt env = function
(* Block
get scope, which is parent and variables.
stuck here. *)
  Ast.Block(stmtlist) ->
    let scopeT = { parent = Some(env.scope); variables = [] } in
    let envT = { env with scope = scopeT} in
    let stmtlist = List.map (fun s -> (check_stmt env s)) stmtlist in 
    envT.scope.variables <- List.rev scopeT.variables;
    Block(stmtlist, envT)
  | Ast.Expr(e) -> Expr(check_expr env e)
  | Ast.Func(f) -> Expr((Id("dummy"), Int)) (*ERROR*)
  | Ast.Return(e) -> Return(check_expr env e)
  | Ast.If(e, s1, s2) ->
    let (e, typ) = check_expr env e in
    if typ != Boolean 
    then raise (Failure ("If stmt does not support this type"));
    If((e, typ), check_stmt env s1, check_stmt env s2)
  | Ast.While(e, s) ->
    let (e, typ) = check_expr env e in
    if typ != Boolean
    then raise (Failure ("While stmt does not support this type"));
    While((e, typ), check_stmt env s)
  | Ast.For(e1, e2, e3, s) -> Expr((Id("dummy"), Int)) 
    (* ERROR:
    let (e1, typ1) = check_expr env e1
    let (e2, typ2) = check_expr env e2
    let (e3, typ3) = check_expr env e3
    *)

(* checking expr and stmt ends here *)


(* Func for initiating environment.
environment is scope and return type. scope is parent and variables.
may want to add more attributes.
*)    
let init_env : environment =
  let init_scope = 
  {
    parent = None;
    variables = [] 
  }
  in { scope = init_scope; return = None; }

(* outter-most function that is called in manit.ml
in: (bind_global list, functions, statements)
out: same triple in SAST types, semantically checked.
*)
let check_program program =
  let env = init_env
  in let stmt_list = program.Ast.stmt_list
  and let func_decl_list = program.Ast.func_decl_list 
  in (stmt_list, func_decl_list)
    

     

(********************** MicroC *************************)
(* first part of MicroC code starts here. 
May need to reference it to transfer functionalities

(* Semantic checking of a program. Returns void if successful,
   throws an exception if something is wrong.

   Check each global variable, then check each function *)


let check (globals, functions, pstmts) =
(* let check (globals, functions) = *)

  (* Raise an exception if the given list has a duplicate *)
  let report_duplicate exceptf list =
    let rec helper = function 
	n1 :: n2 :: _ when n1 = n2 -> raise (Failure (exceptf n1))
      | _ :: t -> helper t
      | [] -> ()
    in helper (List.sort compare list) 
    (* Is compare a type? list of compares? *)
  in

  (* Raise an exception if a given binding is to a void type *)
  let check_not_void exceptf = function
      (Void, n) -> raise (Failure (exceptf n))
    | _ -> ()
  in
  
  (* Raise an exception of the given rvalue type cannot be assigned to
     the given lvalue type *)
  let check_assign lvaluet rvaluet err =
     if lvaluet == rvaluet then lvaluet else raise err
     (* what is diff btw == and = ? *)
  in
   
  (**** Checking Global Variables ****)

  List.iter (check_not_void (fun n -> "illegal void global " ^ n)) globals;
   
  report_duplicate (fun n -> "duplicate global " ^ n) (List.map snd globals);

  (**** Checking Functions ****)

  if List.mem "print" (List.map (fun fd -> fd.fname) functions)
  then raise (Failure ("function print may not be defined")) else ();

  report_duplicate (fun n -> "duplicate function " ^ n)
    (List.map (fun fd -> fd.fname) functions);

  (* Function declaration for a named function *)
  (* SMap.add takes "printf", {}, (). They are key, val, initial? *) 
  let built_in_decls =  StringMap.add "print"
     { typ = Void; fname = "print"; formals = [(Int, "x")];
       locals = []; body = [] } (StringMap.singleton "printb"
     { typ = Void; fname = "printb"; formals = [(Bool, "x")];
       locals = []; body = [] })
   in
  
  (* f_decls is a Map from (f (f map function1) function2)... *)
  let function_decls = List.fold_left (fun m fd -> StringMap.add fd.fname fd m)
                         built_in_decls functions
  in
  
  (* finds "main" *) 
  let function_decl s = try StringMap.find s function_decls
       with Not_found -> raise (Failure ("unrecognized function " ^ s))
  in

  let _ = function_decl "main" in (* Ensure "main" is defined *)

  (* checks formals, fname, locals *)
  let check_function func =

    List.iter (check_not_void (fun n -> "illegal void formal " ^ n ^
      " in " ^ func.fname)) func.formals;

    report_duplicate (fun n -> "duplicate formal " ^ n ^ " in " ^ func.fname)
      (List.map snd func.formals);

    List.iter (check_not_void (fun n -> "illegal void local " ^ n ^
      " in " ^ func.fname)) func.locals;

    report_duplicate (fun n -> "duplicate local " ^ n ^ " in " ^ func.fname)
      (List.map snd func.locals);

    (* Type of each variable (global, formal, or local *)
    let symbols = List.fold_left (fun m (t, n) -> StringMap.add n t m)
	StringMap.empty (globals @ func.formals @ func.locals )
    in

    let type_of_identifier s =
      try StringMap.find s symbols
      with Not_found -> raise (Failure ("undeclared identifier " ^ s))
    in


(* two functions - expr and stmt - are from Microc w/o type inf. 
    (* Return the type of an expression or throw an exception *)
    let rec expr = function
	Literal _ -> Int
      | BoolLit _ -> Bool
      | Id s -> type_of_identifier s
      | Binop(e1, op, e2) as e -> let t1 = expr e1 and t2 = expr e2 in
	(match op with
          Add | Sub | Mult | Div when t1 = Int && t2 = Int -> Int
	| Equal | Neq when t1 = t2 -> Bool
	| Less | Leq | Greater | Geq when t1 = Int && t2 = Int -> Bool
	| And | Or when t1 = Bool && t2 = Bool -> Bool
        | _ -> raise (Failure ("illegal binary operator " ^
              string_of_typ t1 ^ " " ^ string_of_op op ^ " " ^
              string_of_typ t2 ^ " in " ^ string_of_expr e))
        )
      | Unop(op, e) as ex -> let t = expr e in
	 (match op with
	   Neg when t = Int -> Int
	 | Not when t = Bool -> Bool
         | _ -> raise (Failure ("illegal unary operator " ^ string_of_uop op ^
	  		   string_of_typ t ^ " in " ^ string_of_expr ex)))
      | Noexpr -> Void
      | Assign(var, e) as ex -> let lt = type_of_identifier var
                                and rt = expr e in
        check_assign lt rt (Failure ("illegal assignment " ^ string_of_typ lt ^
				     " = " ^ string_of_typ rt ^ " in " ^ 
				     string_of_expr ex))
      | Call(fname, actuals) as call -> let fd = function_decl fname in
         if List.length actuals != List.length fd.formals then
           raise (Failure ("expecting " ^ string_of_int
             (List.length fd.formals) ^ " arguments in " ^ string_of_expr call))
         else
           List.iter2 (fun (ft, _) e -> let et = expr e in
              ignore (check_assign ft et
                (Failure ("illegal actual argument found " ^ string_of_typ et ^
                " expected " ^ string_of_typ ft ^ " in " ^ string_of_expr e))))
             fd.formals actuals;
           fd.typ
    in

    let check_bool_expr e = if expr e != Bool
     then raise (Failure ("expected Boolean expression in " ^ string_of_expr e))
     else () in

    (* Verify a statement or throw an exception *)
    let rec stmt = function
	Block sl -> let rec check_block = function
           [Return _ as s] -> stmt s
         | Return _ :: _ -> raise (Failure "nothing may follow a return")
         | Block sl :: ss -> check_block (sl @ ss)
         | s :: ss -> stmt s ; check_block ss
         | [] -> ()
        in check_block sl
      | Expr e -> ignore (expr e)
      | Return e -> let t = expr e in if t = func.typ then () else
         raise (Failure ("return gives " ^ string_of_typ t ^ " expected " ^
                         string_of_typ func.typ ^ " in " ^ string_of_expr e))
           
      | If(p, b1, b2) -> check_bool_expr p; stmt b1; stmt b2
      | For(e1, e2, e3, st) -> ignore (expr e1); check_bool_expr e2;
                               ignore (expr e3); stmt st
      | While(p, s) -> check_bool_expr p; stmt s
    in

    stmt (Block func.body)
   
  in
  List.iter check_function functions
*)



