(* Semantic checking for the MicroC compiler *)
open Sast

module StringMap = Map.Make(String)

(* Semantic checking of a program. Returns void if successful,
   throws an exception if something is wrong.

   Check each global variable, then check each function *)
let type_to_str = function
  Int -> "int"
  | String -> "String"
  | Void -> "void"
  | Bool -> "bool"

(*AST to SAST*)
let rec check_expr env global_env = function
  Ast.IntLiteral(l) -> Literal(l), Int
  | Ast.FloatLiteral(l) -> Literal(l), Float
  | Ast.BoolLit(l) -> Literal(l), Bool
  | Ast.StringLit(l) -> Literal(l), String
  | Ast.Id(v) ->
    let vdecl = try
      find env.scope v
    with Not_found ->
      raise (Failure("Undeclared Identifier " ^ v)) in
    let (v, typ) = vdecl in
    Id(v), typ
  | Binop(e1, op, e2) as e -> let t1 = expr e1 and t2 = expr e2 in
    (match op with
      Add | Sub | Mul | Div when t1 = Int && t2 = Int -> Int
      |   Add | Sub | Mul | Div when t1 = Float && t2 = Float -> Float
      | Equal | Neq when t1 = t2 -> Bool
      | Less | Leq | Greater | Geq when t1 = Int && t2 = Int -> Bool
      | Less | Leq | Greater | Geq when t1 = Float && t2 = Float -> Bool
      | And | Or when t1 = Bool && t2 = Bool -> Bool
      | _ -> raise (Failure ("Illegal binary operator "))
    )
  | Ast.Binop(e1, op, e2) ->
    let e1 = check_expr env global_env e1
    and e2 = check_expr env global_env e2 in

    let _, t1 = e1
    and _, t2 = e2 in
    (*Operators come in*)
    let return_type = get_binop_type t1 op t2 in
    Binop(e1,op,e2),return_type
(*   | Ast.Assign(v, assignee) ->
    let assign_info = {id=v;assign_scope=env.scope;nesting=0} in
    let assignee_env = match assignee with
      Call(_) -> {env with return_assigner= (Some assign_info) }
      | _ -> env
    in
    let (assignee_e, assignee_type) as assignee = check_expr assignee_env global_env assignee in
    let expr_promise = get_assignment_expression_promise assign_info global_env assignee_e assignee_type in
    assert_not_void assignee_type "Can't assign void to a variable.";
    let (new_e,new_type) as vdecl =
      try (*Reassigning a variable to a different type is okay because assigment = declaration*)
        let (_,prev_typ) = find env.scope v in (*Add it in the symbol table if its a different type*)
        if (not (can_assign prev_typ assignee_type)) then
          raise (Failure ("identifier type cannot be assigned to previously declared type " ^ v))
        else
          Assign (v, expr_promise), assignee_type
    with Not_found -> (*Declaring/Defining a new variable*)
      let decl = (v, assignee_type) in env.scope.variables <- (decl :: env.scope.variables) ;
      VAssign (v, expr_promise), assignee_type
  in
  (if (is_table new_type) then
    create_assignment_linkage_if_applicable v 0 env.scope assignee_e;
    update_table_type env.scope v new_type);
  vdecl *)


and check_stmt env global_env = function
  Ast.Block(sl) ->
    let scopeT = { parent = Some(env.scope); variables = []; update_table_links=[] } in
    let envT = { env with scope = scopeT} in
    let sl = List.map (fun s -> (check_stmt envT global_env s)) sl in envT.scope.variables <- List.rev scopeT.variables;
    Block (sl, envT)
  | Ast.Expr(e) ->
      (match e with
      Assign(_) | Call(_) -> Expr(check_expr env global_env e)
      | _ -> raise (Failure("Expression is not statement in ManiT")))
(*   | Ast.Func(f) ->(
    try (*Test to see if user is trying to overwrite built-in function*)
      ignore(find_built_in f.Ast.fname) ;
      raise (Failure("function is overwrites built-in function " ^ f.Ast.fname))
    with Not_found -> (*valid function*)
      Block([], env )
    ) *)


let rec range a b =
  if a=b-1 then
    [a]
  else
    a::(range (a+1) b)
    
let check_duplicates compare lst = 
  let sorted = List.sort compare lst in
  match sorted with 
    [] -> false
    | hd::tl -> fst (List.fold_left (fun (b,last) next -> (b || last=next),next) (false,hd) tl)
    
(*Are all the elements of a list the same? *)
let all_the_same = function
  | [] -> true
  | lst ->
    (let hd = (List.hd lst) in
    List.for_all ((=) hd) lst)

let check_func_decls_stmt stmt_list1 =
  let rec get_func_decls_stmt_unchecked stmt=
    match stmt_list1 with
      Ast.Block(stmt_list) -> List.concat (List.map get_func_decls_stmt_unchecked stmt_list)
      | Ast.Func(fdecl) -> [fdecl.fname,fdecl]
      | _ -> []
  in
  let func_decls = get_func_decls_stmt_unchecked stmt in
  (*Make sure that there are no duplicates*)
  let names = List.map fst func_decls in
  if (check_duplicates String.compare names) then
    raise (Failure "Duplicate function names!")
  else
    func_decls

let check_program p =
(*   check if there are duplicates in function names*)  
let func_decls = check_func_decls p.Ast.full_program in

  let init_scope = {
    parent = None;
    variables = [];
  } in
  let init_env = { 
    scope = init_scope;
    return = None;
    func_decls = func_decls;
  } in
  let global_env = { funcs = []; finished=false} in

  let env = {env} in
  let typed_program = List.map check_stmt env global_env p.Ast.full_program in
  global_env.finished <- true;

  {typed_program = typed_program;}
