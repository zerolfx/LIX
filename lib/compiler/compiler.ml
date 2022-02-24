open Compiler_common
module A = Last


let build_struct_field_ptr ?(name : string = "struct_field_ptr") (ptr : L.llvalue) (field : int) : L.llvalue = 
  L.build_struct_gep ptr field name builder

let build_load_struct_field_ptr ptr field = 
  L.build_load (build_struct_field_ptr ptr field) "load_ptr" builder


let codegen_primitive = function
| Type.Int i -> store_void_ptr (L.const_int int_type i) builder
| Type.Bool b -> store_void_ptr (L.const_int bool_type (if b then 1 else 0)) builder


let gen_env_type (n : int) : L.lltype =
  L.struct_type context (Array.make n void_ptr_type)
  

let rec codegen (var_table : L.llvalue M.t) (a: A.last) : L.llvalue = match a with
| A.Primitive p -> codegen_primitive p

| A.Variable name -> 
  if String.starts_with ~prefix:"__llvm" name then
    Builtins.codegen_builtin var_table name
  else if String.starts_with ~prefix:"__builtin" name then
    L.build_load (L.lookup_global name the_module |> Option.get) "load_global" builder
  else 
    (match M.find_opt name var_table with
    | Some v -> v
    | None -> L.build_load (L.lookup_global name the_module |> Option.get) "load_global" builder)

| A.Define (name, ast) ->
  let global = declare_global name void_ptr_type in
  L.build_store (codegen var_table ast) global builder |> ignore;
  L.build_load global "load_global" builder

| A.Lambda (name, free_vars, arg, body) -> 
  let env_type = gen_env_type (List.length free_vars) in
  let env_ptr = build_malloc_ptr env_type in

  let f = L.declare_function name function_type the_module in

  let closure_ptr = build_malloc_ptr closure_struct in

  
  List.iteri (fun i v ->
    L.build_store 
      (codegen var_table (A.Variable v)) 
      (build_struct_field_ptr env_ptr i) builder |> ignore) free_vars;

  L.build_store 
    f
    (build_struct_field_ptr ~name:"f_ptr" closure_ptr 0 ) builder |> ignore;

  L.build_store
    (L.build_bitcast env_ptr void_ptr_type "env_ptr" builder)
    (build_struct_field_ptr ~name:"env_ptr" closure_ptr 1) builder |> ignore;
  
  
  let old_block = L.insertion_block builder in
  let block = L.append_block context "entry" f in
  L.position_at_end block builder;


  let f_env_ptr = L.build_bitcast (L.param f 1) (L.pointer_type env_type) "f_env_ptr" builder in

  let table = 
    (arg, L.param f 0) ::
    List.mapi (fun i n -> 
      (n, build_load_struct_field_ptr f_env_ptr i)
    ) free_vars |> List.to_seq |> M.of_seq  in

  L.build_ret (codegen table body) builder |> ignore;
  
  verify_and_optimize f;

  L.position_at_end old_block builder;  
  L.build_bitcast closure_ptr void_ptr_type "closure_ptr" builder

| A.Application (f, arg) -> 
  let closure_ptr = L.build_bitcast (codegen var_table f) closure_ptr_type "closure_ptr" builder in
  let f_ptr = build_load_struct_field_ptr closure_ptr 0 in
  let env_ptr = build_load_struct_field_ptr closure_ptr 1 in
  L.build_call f_ptr [| codegen var_table arg ; env_ptr |] "call" builder

| A.If (c, e1, e2) ->
  let c_val = codegen var_table c in

  let start_bb = L.insertion_block builder in
  let the_function = L.block_parent start_bb in

  let true_bb = L.append_block context "true" the_function in
  L.position_at_end true_bb builder;
  let true_val = codegen var_table e1 in
  let new_true_bb = L.insertion_block builder in

  let false_bb = L.append_block context "false" the_function in
  L.position_at_end false_bb builder;
  let false_val = codegen var_table e2 in
  let new_false_bb = L.insertion_block builder in
  
  let end_bb = L.append_block context "end" the_function in
  L.position_at_end end_bb builder;
  let phi = L.build_phi [(true_val, new_true_bb); (false_val, new_false_bb)] "if_result" builder in
  
  L.position_at_end start_bb builder;
  L.build_cond_br (load_void_ptr bool_type c_val builder) true_bb false_bb builder |> ignore;
  
  L.position_at_end new_true_bb builder;
  L.build_br end_bb builder |> ignore;
  L.position_at_end new_false_bb builder;
  L.build_br end_bb builder |> ignore;
  
  L.position_at_end end_bb builder;
  phi
