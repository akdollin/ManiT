(* Semantic checking for the MicroC compiler. 
checks semantics of AST and returns SAST. *)

open Sast
module A = Ast
module StringMap = Map.Make(String)

let built_in = [("print", A.String, A.Int)]

let global_env = { funcs = [] }

let structs_hash:(string, A.strc) Hashtbl.t = Hashtbl.create 10
(* let struct_func_hash:(string, A.func) Hashtbl.t = Hashtbl.create 10 *)

(* whether t2 is assignable to t1. Add rules as necessary *)
let is_assignable t1 t2 = match t1, t2 with
      t1, t2 when t1 = t2 -> true
      (* add tables *)
    | _ -> false
(* let is_assignable t1 t2 = if t1 = t2 then true else false *)

let all_the_same = function
    [] -> true
  | lst -> let hd = (List.hd lst) in List.for_all ((=) hd) lst

(* finds var in scope *)
let rec find_var scope name = try
  (*List.find ('a -> bool) -> a' list
    finds first element in a' list that satisfies predicate (a' -> bool) *)
  List.find (fun (s, _) -> s = name) scope.variables with Not_found ->
  (*if not found in our scope, try parent's scope or raise not found *)
  match scope.parent with
    (* parent is also a scope. if parent is None, do nothing. *)
      Some(parent) -> find_var parent name
    | _ -> raise Not_found

let find_built_in name = try
  List.find (fun (id, typ, ret) -> id = name) built_in with Not_found -> raise Not_found

let find_func name = try
  List.find (fun f -> f.fname = name) global_env.funcs with Not_found -> raise Not_found  

let exist_func name = try
  List.find (fun f -> f.fname = name) global_env.funcs; true with Not_found -> false

let check_duplicate_struct strctName =
(* Hashtbl.find structs_hash strct; true with Not_found -> false *)
  try Hashtbl.find structs_hash strctName; true with Not_found -> false

(* Helper function to check for dups in a list *)
let report_duplicate exceptf list =
    let rec helper = function
        n1 :: n2 :: _ when n1 = n2 -> raise (Failure (exceptf n1))
      | _ :: t -> helper t
      | [] -> ()
    in helper (List.sort compare list)

    (* let exist_struct name = try
  List.find (fun s -> s.sname = name) global_structs.structs; true with Not_found -> false *)
let check_valid_struct s =
  try Hashtbl.find structs_hash s
  with | Not_found -> raise (Exceptions.InvalidStruct s)

(*check_expr: core type-matching function that recursively annotates type of each expr. *)
let rec check_expr (env : environment) = function
  (* literals *)
    Ast.IntLit(value) -> IntLit(value), A.Int
  | Ast.FloatLit(value) -> FloatLit(value), A.Float
  | Ast.StringLit(value) -> StringLit(value), A.String
  | Ast.BoolLit(value) -> BoolLit(value), A.Bool

  (* Variable access *)
  | Ast.Id(name) -> let (name, typ) = try find_var env.scope name with 
      Not_found -> raise (Failure("undeclared identifier! " ^ name)) in 
      Id(name), typ
  
  (* Assignment(string, expr)
  checks expr of R.H.S, and compares type of expr to that of L.H.S from its declaration. 
  populates scope's variable if not found.
  modified from hawk. need testing. 
  need to add rules/function for promotion/demotion here. *)
  | Ast.Assign(lhs, expr) ->
    let thing = match lhs with 
      A.Id(name) -> 
        let (expr, right_typ) = check_expr env expr in (* R.H.S typ *)
        let sast_assign = (* (n, (e, e's typ)), n's typ *) 
        try let (name, left_typ) = find_var env.scope name in
          if left_typ <> right_typ (* type mismatch. depends on rule. *)
          then raise (Failure (" type mismatch "))
          else Assign((Id(name), left_typ), (expr, right_typ)), right_typ
        with Not_found -> (* new name. declaration. *)
          let decl = (name, right_typ) in 
          env.scope.variables <- (decl :: env.scope.variables);
          Assign((Id(name), right_typ), (expr, right_typ)), right_typ
        in sast_assign
      | A.Array_access(arrayName,index) -> 
        let (expr, right_typ) = check_expr env expr in (* R.H.S typ *)
        let (arr, left_typ) = check_expr env lhs in
          if left_typ <> right_typ (* type mismatch. depends on rule. *)
          then raise (Failure (" type mismatch in array assign"))
          else Assign((strct, left_typ), (expr, right_typ)), right_typ
      | A.Struct_access(structName, field) -> 
        let (expr, right_typ) = check_expr env expr in (* R.H.S typ *)
        let (strct, left_typ) = check_expr env lhs in
          if left_typ <> right_typ (* type mismatch. depends on rule. *)
          then raise (Failure (" type mismatch in struct assign"))
          else Assign((strct, left_typ), (expr, right_typ)), right_typ
      | _ -> raise(Failure("Not a valid assign: We only allow id, struct, and array assign"))

    in thing
  (* Binop(expr, op, expr)
  checks types of L.H.S and R.H.S
  we need to specify rules for promotion/demotion of OPs *)
  | Ast.Binop (e1, op, e2) ->
    let e1 = check_expr env e1
    and e2 = check_expr env e2 in
    
    let _, t1 = e1
    and _, t2 = e2 in
    
    let binop_type t1 op t2 = match op with
        A.Add | A.Sub | A.Mult | A.Div ->
        ( match t1, t2 with
          A.Int, A.Int -> A.Int
        | A.Float, A.Float -> A.Float
        | _, _ -> raise(Failure("binary op type mismatch")))
      | A.Less | A.Leq | A.Greater | A.Geq -> 
        (match t1, t2 with
          A.Int, A.Int -> A.Bool
        | A.Float, A.Float -> A.Bool
        | _, _ -> raise(Failure("binary op type mismatch")))
      | A.Equal | A.Neq ->
        (match t1, t2 with
          A.Int, A.Int -> A.Bool
        | A.Float, A.Float -> A.Bool (* float comparison ok? *)
        | A.Bool, A.Bool -> A.Bool
        | _, _ -> raise(Failure("binary op type mismatch")))
      | A.And | A.Or ->
        (match t1, t2 with
          A.Bool, A.Bool -> A.Bool
        | _, _ -> raise(Failure("binary op type mismatch")))
      | _ -> raise(Failure("binop can't handle these ops."))

    in let typ = binop_type t1 op t2 in
    Binop(e1, op, e2), typ
      
  (* Unop(uop, expr)
  uop is either Neg or Not*)
  | Ast.Unop(uop, e) ->
      let (e, typ) = check_expr env e in (
      match uop with
        A.Neg -> 
          (if typ != A.Int && typ != A.Float
          then raise(Failure("unary minus opeartion does not support this type ")));
          Unop(uop, (e, typ)), typ
      | A.Not ->
          (if typ != A.Bool
          then raise(Failure("unary not operation does not support this type ")));
          Unop(uop, (e, typ)), typ
      | _ -> raise(Failure("unop error"))
      )
  
  (* Function Call *)
  | Ast.Call(name, actuals) -> (
    (* check types to each actuals and get types of formals from fdecl. *)
    let typed_actuals = List.map (fun e -> (check_expr env e)) actuals in
    match name with
      | "print" -> Call("print", typed_actuals), A.Int
      | _ -> (* non-print functions *) (
        let func = try find_func name with Not_found ->
          raise(Failure("undefined function was called.")) in

        (* unused?
        let match_arg_num formals actuals =
            if List.length formals <> List.length actuals then
            raise(Failure("number of actuals and formals do not match")) in
        *)

        let match_types formals actuals = match formals, actuals with
          | (ftyp, _) :: _, ( _ , atyp) :: _ ->
            if not(is_assignable ftyp atyp) then raise(Failure("typ of actuals do not match those of formals"));
            if not(List.length formals = List.length actuals) then
            raise(Failure("number of actuals and formals do not match")); ()
          | _, _ -> if not(List.length formals = List.length actuals) then
                    raise(Failure("number of actuals and formals do not match"))
        
        in match_types func.formals typed_actuals;
        Call(name, typed_actuals), func.typ (* return name and f_typ from fdecl *)
      ))
  | Struct_access(var_expr, attr) ->
      (* convert to str *)
      let var = (match var_expr with 
          A.Id(s) -> s
        | _ -> raise(Failure("struct access: complex vars not supported as of now."))) in

      let (var, A.Struct_typ(sname)) = find_var env.scope var in  (* find the instance of struct that was declared in current scope *)
      let strc = Hashtbl.find structs_hash sname in (* find strc type definition *)
      let (typ, _) = List.find (fun (_, id) -> id = attr) strc.vdecls in (* typ of attr *)
      (* find index of attr in struct. this index is used in codegen *)
      let rec index_of_list x l = (match l with
          hd::tl -> let (_,id) = hd in if id = x then 0 else 1 + index_of_list x tl
        | _ -> raise(Failure("index_of_list failed"))) in
      let index = index_of_list attr strc.vdecls in
      Struct_access(var, attr, index), typ
      (* need to handle each error separately for robustness *)
  
  | Array_create(expr_list) ->
      let length = List.length expr_list in
      let checked_expr_list = List.map (fun expr -> check_expr env expr) expr_list in
      let typs = List.map (fun (_,typ) -> typ) checked_expr_list in
      (match all_the_same typs with
        false -> raise(Failure("typs elements in array are not coherent"))
      | true -> Array_create(checked_expr_list), Ast.Array_typ(List.hd typs, length))

  | Array_access(var_expr, index_expr) ->
      (* convert to str *)
      let var = (match var_expr with
          A.Id(s) -> s
        | _ -> raise(Failure("array access: complex vars not supported as of now."))) in 
      (* find var first *)
      let (var, A.Array_typ(var_typ, length)) = try find_var env.scope var with Not_found ->
        raise(Failure("array variable not found")) in
      (* need to check if arr typ *)
      (* check index expr *)
      let (index_expr, index_expr_typ) = check_expr env index_expr in
      if index_expr_typ != A.Int then raise(Failure("array access requires int arg")) 
      (* need separate function to evaluate the expr.
         we only allow int literal (not binop, unop) for now *)
      else match index_expr with
          IntLit(index) -> 
            if index < 0 || index > length - 1 
            then raise(Failure("access out of bounds"))
            else Array_access(var, index), var_typ (* no need to return the value here! *)
        | _ -> raise(Failure("array access: only int lit allowed for now"))




(* gets return types from checked stmts with typed expressions *)
let rec get_return_types typ_list stmt = match stmt with
    Return((e, t)) -> t :: typ_list 
  | Block(sl) -> List.fold_left get_return_types typ_list sl
  | If(_, s1, s2) -> (get_return_types typ_list s1) @ (get_return_types typ_list s2)
  | While (_, s) -> (get_return_types typ_list s)
  | For (_, _, _, s) -> (get_return_types typ_list s)
  | _ -> typ_list

(* checks typ of func from fdecl with those in fbody *)
let check_return_types func_typ func_body =
  let ret_typs = List.fold_left get_return_types [] func_body in
  List.iter (fun each_ret_typ -> (if (each_ret_typ != func_typ) 
  then raise(Failure("return types in fbody do not match with fdecl"))); ) ret_typs

(* check_stmt *)
let rec check_stmt env = function
  | Ast.Expr(e) -> Expr(check_expr env e)
  | Ast.Return(e) -> Return(check_expr env e)
  | Ast.Block(stmtlist) ->
    (* sets a new scope to scope passed in *)
    let new_scope = { parent = Some(env.scope); variables = [] } in
    let new_env = { scope = new_scope } in
    (* populates variables and annotates exprs by calling check_stmt *)
    (* adds new env to all stmts *)
    let stmtlist = List.map (fun s -> check_stmt new_env s) stmtlist in 
    (* setting to *)
    new_env.scope.variables <- List.rev new_scope.variables;
    Block(stmtlist) (* new_env *)
  
  (* Func.
  checks env. checks if all return types match with fdecl. adds fdecl to env. *)
  | Ast.Func(func) ->
    (* add fdecl to global env if it hasn't declared previously. *)
    ( match exist_func func.fname with 
      false ->
        (* make new scope and env with formals *)
        let flipped_formals = List.map (fun (t, id) -> (id, t)) func.formals in
        let new_scope = { parent = Some(env.scope); variables = flipped_formals } in
        let new_env = { scope = new_scope } in
        (* iterate thru stmtlist like a block *)
        let sast_fbody = List.map (fun stmt -> check_stmt new_env stmt) func.body in
        let sast_func = { typ = func.typ; fname = func.fname; formals = func.formals; body = sast_fbody } in
        global_env.funcs <- (sast_func :: global_env.funcs);
        (* check return typs within fbody and ftype. calls check_expr again on return stmts. *)
        check_return_types func.typ sast_fbody;
        Func(sast_func)
    | true -> raise(Failure("cannot redeclare function with same name")); )
  (* struct stmt *)
  | Ast.Struc(strc) ->
    (*  ignore(check_duplicate_struct strc); *)    
    (match check_duplicate_struct strc.A.sname with 
      false -> 
        let check_fields = report_duplicate (fun n -> "duplicate struct field " ^ n) (List.map (fun n -> snd n) strc.A.vdecls) in
        let struct_sast = { sname = strc.sname; vdecls = strc.vdecls } in
        Hashtbl.add structs_hash strc.A.sname strc;
        Struc(struct_sast)
    | true -> raise(Failure("cannot redeclare struct with same name"));)

  | Ast.Vdecl(typ, name) -> 
    (match typ with (* check if struct typ *)
      Struct_typ(strc_name) -> 
        try find_var env.scope name; (* if var name already exists *)
        raise(Failure("Cannot redeclare an existing variable name!")) with 
        Not_found -> 
            (match check_duplicate_struct strc_name with (* check if struct typ exists *)
              true -> (* add to scope variables and return Vdecl *)
                let decl = (name, typ) in
                env.scope.variables <- (decl :: env.scope.variables);
                Vdecl(typ,name)
            | false -> raise(Failure("Struct not declared!")))
      | _ -> raise(Failure("ManiT is type inferred, you dun messed up!")))

  (* conditionals *)
  | Ast.If(e, s1, s2) ->
      let (e, typ) = check_expr env e in
      (if typ != Bool then raise (Failure ("If stmt does not support this type")));
      If((e, typ), check_stmt env s1, check_stmt env s2)
  | Ast.While(e, s) ->
      let (e, typ) = check_expr env e in
      (if typ != A.Bool then raise (Failure ("While stmt does not support this type")));
      While((e, typ), check_stmt env s)
  | Ast.For(e1, e2, e3, s) -> 
      let (e1, typ1) = check_expr env e1 (*need to have empty expr *)
      and (e2, typ2) = check_expr env e2
      and (e3, typ3) = check_expr env e3 in
      (if typ2 != Bool then raise(Failure("For stmt does not support this type")));
      For((e1, typ1), (e2, typ2), (e3, typ3), check_stmt env s)
  | _ -> raise(Failure("unchecked stmts"))

(* environment is a record with scope and return type. 
scope is subrecord with parent and variables.
may want to add more attributes to records. *)    
let init_env =
  let init_scope  = {
    parent = None;
    variables = [];
  }
  in { scope = init_scope; }

(* outter-most function that is called in manit.ml
in: (bind_global list, functions, statements)
out: same triple in SAST types, semantically checked.
*)

let check_program program =
  let env = init_env in
  List.map (fun stmt -> check_stmt env stmt) program
