open Compiler_common

let codegen_builtin (var_table : L.llvalue M.t) (name : string) : L.llvalue = match name with
| "__llvm__add" -> L.build_add (M.find "a" var_table) (M.find "b" var_table) "add" builder
| "__llvm__printi" -> 
    let printi = L.declare_function "__printi" (L.function_type int_type [| int_type |]) the_module in
    L.build_call printi [| (M.find "a" var_table) |] "call_printi" builder
| _ -> raise (Failure "unsupported builtin")


let builtin = [
  ("__builtin_add2", 
    A.Lambda ("__builtin_lam_add2", [], ("a", Type.IntT), 
      A.Lambda ("__builtin_lam_add1", [("a", Type.IntT)], ("b", Type.IntT), 
        A.Var ("__llvm__add", Type.IntT))));
  ("__builtin_printi",
    A.Lambda ("__builtin_lam_printi", [], ("a", Type.IntT), 
      A.Var ("__llvm__printi", Type.IntT)))
]


let build_init_builtins (codegen : A.last -> L.llvalue) : L.llvalue = 
  let init = L.declare_function "init_builtins" (L.function_type void_type [||]) the_module in
  let bb = L.append_block context "entry" init in
  L.position_at_end bb builder;

  let gen_builtin (name, c) =
    let global_closure = L.define_global name (L.const_pointer_null closure_ptr_type) the_module in
    L.set_linkage L.Linkage.External_weak global_closure;
    L.build_store (codegen c) global_closure builder |> ignore in
  List.iter gen_builtin builtin;
  
  L.build_ret_void builder |> ignore;

  init