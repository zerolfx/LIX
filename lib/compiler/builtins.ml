open Compiler_common
open Builtin_constants


let rec get_args_count = function
| Type.FunctionT (_, ty) -> 1 + get_args_count ty
| _ -> 0


let gen_builtin_arg_name i = "arg" ^ string_of_int i

let rec gen_builtin_ast (args : Common.typed_var list) (name : string) (ty : Type.t) = match ty with
| Type.FunctionT (a, e) -> 
    let depth = List.length args in
    let arg = (gen_builtin_arg_name depth, a) in
    A.Lambda (name ^ "_" ^ Int.to_string depth, args, arg, gen_builtin_ast (arg::args) name e)
| IntT | BoolT -> A.Var (llvm_prefix ^ name, ty)
| _ -> assert false



let codegen_builtin (var_table : L.llvalue M.t) (name : string) : L.llvalue =
  let i_name = Str.string_after name (String.length llvm_prefix) in
  let b = List.find (fun { internal_name; _ } -> String.equal internal_name i_name ) builtins in
  b.gen (List.init (get_args_count b.ty) (fun i -> M.find (gen_builtin_arg_name i) var_table)) builder


let build_init_builtins (codegen : A.last -> L.llvalue) : L.llvalue = 
  let init = L.declare_function "init_builtins" (L.function_type void_type [||]) the_module in
  let bb = L.append_block context "entry" init in
  L.position_at_end bb builder;

  let gen_builtin name c =
    let global_closure = declare_global name closure_ptr_type in
    L.build_store (codegen c) global_closure builder |> ignore in

  List.iter (fun { internal_name; ty; _ } -> 
    gen_builtin internal_name (gen_builtin_ast [] internal_name ty)
  ) builtins;

  
  L.build_ret_void builder |> ignore;

  init