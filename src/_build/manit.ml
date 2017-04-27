(* Top-level of the ManiTcompiler: scan & parse the input,
   check the resulting AST, generate LLVM IR, and dump the module *)

let _ =
  let lexbuf = Lexing.from_channel stdin in
  let ast = Parser.program Scanner.token lexbuf in
  let sast = Semant.check_program ast in
  let m = Codegen.translate sast in
  Llvm_analysis.assert_valid_module m;
  print_string (Llvm.string_of_llmodule m)


(* version with options. commented out due to errors in prettyprinter
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
  let sast = Semant.check_program ast in 
  
  (* depends on command line options. default is Compile *)
  match action with
    (* Ast.string_of_program is the pretty-print func in AST. *)
    Ast -> print_string (Prettyprint.string_of_program sast)
  | LLVM_IR -> print_string (Prettyprint.string_of_llmodule (Codegen.translate sast))
    
    (* Codegen translates SAST (or AST) to IR of type module.
       run that module through LLVM analyzer for debugging.
       if valid module, translate module to string and syscall print. *)
  | Compile -> let m = Codegen.translate sast in
    Llvm_analysis.assert_valid_module m;
    print_string (Llvm.string_of_llmodule m)
*)
