(* Top-level of the ManiTcompiler: scan & parse the input,
   check the resulting AST, generate LLVM IR, and dump the module *)

type action = Ast | LLVM_IR | Compile

let _ =
  (* command line options. *)
  let action = if Array.length Sys.argv > 1 then
    List.assoc Sys.argv.(1) [ ("-a", Ast);	(* Print the AST only *)
			      ("-l", LLVM_IR);  (* Generate LLVM, don't check *)
			      ("-c", Compile) ] (* Generate, check LLVM IR *)
  
  (* w/o command line options *)
  else Compile in
  let lexbuf = Lexing.from_channel stdin in (* set input stream *)
  
  (* scan input to tokens, parse the tokens to get AST *) 
  let ast = Parser.program Scanner.token lexbuf in

  (* semantic checker takes AST, checks input, produces SAST *)
  (* let sast = Semant.check_program ast; *)
  
  (* depends on command line options. default is Compile *)
  match action with
    (* Ast.string_of_program is the pretty-print func in AST. 
       may want to refactor it out to prettyprint.ml *)
    Ast -> print_string (Ast.string_of_program ast)
  | LLVM_IR -> print_string (Llvm.string_of_llmodule (Codegen.translate ast))
    
    (* Codegen translates SAST (or AST) to IR of type module.
       run that module through LLVM analyzer for debugging.
       if valid module, translate module to string and syscall print. *)
  | Compile -> let m = Codegen.translate ast in
    Llvm_analysis.assert_valid_module m;
    print_string (Llvm.string_of_llmodule m)
