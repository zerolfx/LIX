open Common
open Compiler_common


let build_malloc_ptr (t : L.lltype) : L.llvalue = 
  L.build_malloc t "malloc" builder


let build_struct_field_ptr ?(name : string = "struct_field_ptr") (ptr : L.llvalue) (field : int) : L.llvalue = 
  L.build_struct_gep ptr field name builder


let codegen_primitive = function
| Ast.Int i -> L.const_int int_type i
| Ast.Bool b -> L.const_int bool_type (if b then 1 else 0)



let gen_llvm_function_type (arg : Type.t) (body : Type.t) : L.lltype =
  L.function_type (gen_type body) [| gen_type arg ; void_ptr_type |]

let gen_env_type (vars : typed_var list) : L.lltype =
  L.struct_type context (List.map (Core.Fn.compose gen_type type_of_var) vars |> Array.of_list)
  


let rec codegen (var_table : L.llvalue M.t) (a: A.last) : L.llvalue = match a with
| A.Primitive p -> codegen_primitive p

| A.Var (name, _) -> 
  if String.starts_with ~prefix:"__llvm" name then
    Builtins.codegen_builtin var_table name
  else if String.starts_with ~prefix:"__builtin" name then
    L.build_load (L.lookup_global name the_module |> Option.get) "load_global" builder
  else 
    (match M.find_opt name var_table with
    | Some v -> v
    | None -> L.build_load (L.lookup_global name the_module |> Option.get) "load_global" builder)

| A.Define (name, ast) ->
  let global = declare_global name (a |> Last.get_type |> gen_type) in
  L.build_store (codegen var_table ast) global builder |> ignore;
  L.build_load global "load_global" builder

| A.Lambda (name, free_vars, arg, body) -> 
  let env_type = gen_env_type free_vars in
  let env_ptr = build_malloc_ptr env_type in

  let f_llvm_type = gen_llvm_function_type (type_of_var arg) (A.get_type body)  in
  let f = L.declare_function name f_llvm_type the_module in

  let closure_ptr = build_malloc_ptr closure_struct in

  
  List.mapi (fun i v ->
    L.build_store 
      (codegen var_table (A.Var v)) 
      (build_struct_field_ptr env_ptr i) builder) free_vars |> ignore;

  L.build_store 
    (L.build_bitcast f void_ptr_type "f_ptr" builder) 
    (build_struct_field_ptr ~name:"f_ptr" closure_ptr 0 ) builder |> ignore;

  L.build_store
    (L.build_bitcast env_ptr void_ptr_type "env_ptr" builder)
    (build_struct_field_ptr ~name:"env_ptr" closure_ptr 1) builder |> ignore;
  
  
  let old_block = L.insertion_block builder in
  let block = L.append_block context "entry" f in
  L.position_at_end block builder;


  let f_env_ptr = L.build_bitcast (L.param f 1) (L.pointer_type env_type) "f_env_ptr" builder in

  let table = 
    (name_of_var arg, L.param f 0) ::
    List.mapi (fun i v -> 
      (name_of_var v, L.build_load (build_struct_field_ptr f_env_ptr i) "env_arg"  builder)
    ) free_vars |>  List.to_seq |> M.of_seq  in

  L.build_ret (codegen table body) builder |> ignore;
  L.position_at_end old_block builder;
    
  verify_and_optimize f;
  Llvm.PassManager.run_function f the_fpm |> ignore;
  
  closure_ptr

| A.Application (_, f, arg) -> (match Last.get_type f with
  | Type.FunctionT (arg_type, body_type) ->
    let closure_ptr = codegen var_table f in
    let f_void_ptr_ptr = build_struct_field_ptr ~name:"f_ptr_ptr" closure_ptr 0 in
    let f_void_ptr = L.build_load f_void_ptr_ptr "f_ptr" builder in
    let env_ptr_ptr = build_struct_field_ptr ~name:"env_ptr" closure_ptr 1 in
    let env_ptr = L.build_load env_ptr_ptr "env_ptr" builder in
    let f_llvm_ptr_type = L.pointer_type (gen_llvm_function_type arg_type body_type) in
    let f_ptr = L.build_bitcast f_void_ptr f_llvm_ptr_type "f_ptr" builder in
    L.build_call f_ptr [| codegen var_table arg ; env_ptr |] "call" builder
  | _ -> raise (Failure "invalid function type")
)



