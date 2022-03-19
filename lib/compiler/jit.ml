open Compiler_common
open Common
module A = Last

let the_execution_engine =
  assert (Llvm_executionengine.initialize());
  Llvm_executionengine.create the_module


(* reinstall the module per run to prevent strange behavior *)
let run f = 
  Llvm_executionengine.add_module the_module the_execution_engine;
  let ret = f () in
  Llvm_executionengine.remove_module the_module the_execution_engine;
  ret


let run_function f t = run @@ fun _ -> Llvm_executionengine.get_function_address (L.value_name f) t the_execution_engine ()

let init_jit () : unit = 
  let init = Builtins.build_init_builtins (Compiler.codegen M.empty) in

  verify_and_optimize init;

  run_function init
    (Foreign.funptr Ctypes.(void @-> returning void))


let gen_type (sc : Type.scheme) : L.lltype = match sc with
| Scheme ([], Type.BoolT) -> bool_type
| Scheme ([], Type.IntT) -> int_type
| _ -> void_type

let codegen_repl (a, sa) : Type.scheme * Type.primitive option =
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
      let res = run_function repl_function 
        (Foreign.funptr Ctypes.(void @-> returning int)) in
      Some (Type.Int res)
    else if llvm_ta = bool_type then
      let res = run_function repl_function 
        (Foreign.funptr Ctypes.(void @-> returning bool)) in
      Some (Type.Bool res)
    else
      let _ = run_function repl_function 
        (Foreign.funptr Ctypes.(void @-> returning void)) in
      None
  in
  
  sa, result