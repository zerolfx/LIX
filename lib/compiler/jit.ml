open Compiler_common
open Common


let the_execution_engine =
  ( match Llvm_executionengine.initialize () with
  | true -> ()
  | false -> raise (Failure "failed to initialize execution engine"));
  Llvm_executionengine.create the_module


let init_jit () : unit =

  Llvm_executionengine.add_module the_module the_execution_engine;

  let init = Builtins.build_init_builtins (Compiler.codegen M.empty) in

  verify_and_optimize init;
  L.dump_module the_module;

  let init_fp = Llvm_executionengine.get_function_address "init_builtins" 
    (Foreign.funptr Ctypes.(void @-> returning void)) 
    the_execution_engine in
  init_fp ();

  Llvm_executionengine.remove_module the_module the_execution_engine

let codegen_repl (a: A.last) : unit = 
  Llvm_executionengine.add_module the_module the_execution_engine;

  let repl_fn = gen_name "repl" in

  let ta = Last.get_type a in
  let repl_function = L.declare_function repl_fn (L.function_type (gen_type ta) [||]) the_module in
  let bb = L.append_block context "entry" repl_function in
  L.position_at_end bb builder;



  L.build_ret (Compiler.codegen M.empty a) builder |> ignore;
  verify_and_optimize repl_function;
  
  L.dump_module the_module;

  Printf.printf "type : %s" (Type.show ta);

  (match ta with
  | Type.IntT -> 
    let repl_fp = Llvm_executionengine.get_function_address repl_fn 
      (Foreign.funptr Ctypes.(void @-> returning int64_t)) 
      the_execution_engine in
    Printf.printf ", eval : %Ld\n" (repl_fp ())
  | _ -> 
    let repl_fp = Llvm_executionengine.get_function_address repl_fn 
      (Foreign.funptr Ctypes.(void @-> returning void)) 
      the_execution_engine in
    repl_fp ();
    print_newline ()
  );

  L.delete_block bb;
  L.delete_function repl_function;

  Llvm_executionengine.remove_module the_module the_execution_engine