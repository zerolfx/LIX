open Compiler_common
open Builtin_constants
module A = Last


let rec get_args_count = function
| Type.FunT (_, ty) -> 1 + get_args_count ty
| _ -> 0


let gen_builtin_arg_var i = "arg" ^ string_of_int i

let rec gen_builtin_ast (args : Type.var list) (name : string) (ty : Type.t) = match ty with
| Type.FunT (_, te) -> 
    let depth = List.length args in
    let arg = gen_builtin_arg_var depth in
    A.Lambda (name ^ "_" ^ Int.to_string depth, args, arg, gen_builtin_ast (arg::args) name te)
| IntT | BoolT -> A.Variable (llvm_prefix ^ name)
| _ -> assert false



let codegen_builtin (var_table : L.llvalue M.t) (name : string) : L.llvalue =
  let i_name = Str.string_after name (String.length llvm_prefix) in
  let b = List.find (fun { internal_name; _ } -> String.equal internal_name i_name ) builtins in
  b.gen (List.init (get_args_count b.ty) (fun i -> M.find (gen_builtin_arg_var i) var_table)) builder


let build_init_builtins (codegen : A.last -> L.llvalue) : L.llvalue = 
  let init = L.declare_function "init_builtins" (L.function_type void_type [||]) the_module in
  let bb = L.append_block context "entry" init in
  L.position_at_end bb builder;

  let gen_builtin name c =
    let global_closure = declare_global name void_ptr_type in
    L.build_store (L.build_bitcast (codegen c) void_ptr_type "global_closure" builder) global_closure builder |> ignore in

  List.iter (fun { internal_name; ty; _ } -> 
    gen_builtin internal_name (gen_builtin_ast [] internal_name ty)
  ) builtins;

  
  L.build_ret_void builder |> ignore;

  init