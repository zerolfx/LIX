open Compiler_common
open Common
module A = Last

let the_execution_engine =
  ( match Llvm_executionengine.initialize () with
  | true -> ()
  | false -> raise (Failure "failed to initialize execution engine"));
  Llvm_executionengine.create the_module


let init_jit () : unit =

  Llvm_executionengine.add_module the_module the_execution_engine;

  let init = Builtins.build_init_builtins (Compiler.codegen M.empty) in

  verify_and_optimize init;

  let init_fp = Llvm_executionengine.get_function_address "init_builtins" 
    (Foreign.funptr Ctypes.(void @-> returning void)) 
    the_execution_engine in
  init_fp ();

  Llvm_executionengine.remove_module the_module the_execution_engine


let gen_type (sc : Type.scheme) : L.lltype = match sc with
| Scheme ([], Type.BoolT) -> bool_type
| Scheme ([], Type.IntT) -> int_type
| _ -> void_type


let codegen_repl (a, sa) : Type.scheme * Type.primitive option = 
  Llvm_executionengine.add_module the_module the_execution_engine;

  let repl_fn = gen_name "repl" in

  let llvm_ta = gen_type sa in
  let repl_function = L.declare_function repl_fn (L.function_type llvm_ta [||]) the_module in
  let bb = L.append_block context "entry" repl_function in
  L.position_at_end bb builder;

  let llvm_result = Compiler.codegen M.empty a in

  if llvm_ta != void_type then
    L.build_ret (load_void_ptr llvm_ta llvm_result builder) builder |> ignore
  else
    L.build_ret_void builder |> ignore;

  verify_and_optimize repl_function;

  let result =
    if llvm_ta = int_type then
      let repl_fp = Llvm_executionengine.get_function_address repl_fn 
        (Foreign.funptr Ctypes.(void @-> returning int)) 
        the_execution_engine in
      Some (Type.Int (repl_fp ()))
    else if llvm_ta = bool_type then
      let repl_fp = Llvm_executionengine.get_function_address repl_fn 
        (Foreign.funptr Ctypes.(void @-> returning bool)) 
        the_execution_engine in
      Some (Type.Bool (repl_fp ()))
    else
      let repl_fp = Llvm_executionengine.get_function_address repl_fn 
        (Foreign.funptr Ctypes.(void @-> returning void)) 
        the_execution_engine in
      repl_fp ();
      None
  in

  Llvm_executionengine.remove_module the_module the_execution_engine;

  sa, result