open Common

module A = Last
module L = Llvm

let context = L.global_context ()
let the_module = L.create_module context "LIX"

let the_execution_engine =
  ( match Llvm_executionengine.initialize () with
  | true -> ()
  | false -> raise (Failure "failed to initialize execution engine"));
  Llvm_executionengine.create the_module

let the_fpm = Llvm.PassManager.create_function the_module
let builder = L.builder context



let int_type = L.i64_type context
let bool_type = L.i1_type context

let void_type = L.void_type context
let void_ptr_type = L.pointer_type void_type

(* 
closure {
  void *function;
  struct env *env;
}
 *)
let closure_struct = L.struct_type context [| void_ptr_type ; void_ptr_type |]
let closure_ptr_type = L.pointer_type closure_struct


let build_malloc_ptr (t : L.lltype) : L.llvalue = 
  L.build_malloc t "malloc" builder


let build_struct_field_ptr ?(name : string = "struct_field_ptr") (ptr : L.llvalue) (field : int) : L.llvalue = 
  L.build_struct_gep ptr field name builder


let codegen_primitive = function
| Ast.Int i -> L.const_int int_type i
| Ast.Bool b -> L.const_int bool_type (if b then 1 else 0)


let gen_type = function
| Type.IntT -> int_type
| Type.BoolT -> bool_type
| Type.FunctionT _ -> closure_ptr_type
| t -> raise (Failure (Printf.sprintf "unsupported type %s" (Type.show t)))

let gen_llvm_function_type (arg : Type.t) (body : Type.t) : L.lltype =
  L.function_type (gen_type body) [| gen_type arg ; void_ptr_type |]

let gen_env_type (vars : typed_var list) : L.lltype =
  L.struct_type context (List.map (Core.Fn.compose gen_type type_of_var) vars |> Array.of_list)
  

module M = Map.Make(String)

let rec codegen (var_table : L.llvalue M.t) (a: A.last) : L.llvalue = match a with
| A.Primitive p -> codegen_primitive p
| A.Var (name, _) -> 
  if String.starts_with ~prefix:"__llvm" name then
    codegen_builtin var_table name
  else if String.starts_with ~prefix:"__builtin" name then
    L.build_load (L.lookup_global name the_module |> Option.get) "load_global" builder
  else M.find name var_table
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
    
  verify_function f;
  Llvm.PassManager.run_function f the_fpm |> ignore;
  
  closure_ptr
| A.Application (_, f, arg) -> (match Last.get_type f with
  | Type.FunctionT (arg_type, body_type) ->
    let closure_ptr = codegen var_table f in
    let f_void_ptr = build_struct_field_ptr ~name:"f_ptr" closure_ptr 0 in
    let env_ptr_ptr = build_struct_field_ptr ~name:"env_ptr" closure_ptr 1 in
    let env_ptr = L.build_load env_ptr_ptr "env_ptr" builder in
    let f_llvm_ptr_type = L.pointer_type (gen_llvm_function_type arg_type body_type) in
    let f_ptr = L.build_bitcast f_void_ptr f_llvm_ptr_type "f_ptr" builder in
    L.build_call f_ptr [| codegen var_table arg ; env_ptr |] "call" builder
  | _ -> raise (Failure "invalid function type")
)
and codegen_builtin (var_table : L.llvalue M.t) (name : string) : L.llvalue = match name with
| "__llvm__add" -> L.build_add (M.find "a" var_table) (M.find "b" var_table) "add" builder
| "__llvm__printi" -> 
    let printi = L.declare_function "__printi" (L.function_type int_type [| int_type |]) the_module in
    L.build_call printi [| (M.find "a" var_table) |] "call_printi" builder
| _ -> raise (Failure "unsupported builtin")


let builtin = 
  [("__builtin_add2", 
    A.Lambda ("__builtin_lam_add2", [], ("a", Type.IntT), 
      A.Lambda ("__builtin_lam_add1", [("a", Type.IntT)], ("b", Type.IntT), 
        A.Var ("__llvm__add", Type.IntT))));
  ("__builtin_printi",
    A.Lambda ("__builtin_lam_printi", [], ("a", Type.IntT), 
      A.Var ("__llvm__printi", Type.IntT)))]


let gen_builtin ((name, c) : string *  A.last) : unit =
  let global_closure = L.declare_global closure_ptr_type name the_module in
  L.build_store (codegen M.empty c) global_closure builder |> ignore


let builtin_table = 
  let init = L.declare_function "init_builtins" (L.function_type void_type [||]) the_module in
  let bb = L.append_block context "entry" init in
  L.position_at_end bb builder;
  
  List.iter gen_builtin builtin;
  

  L.build_ret_void builder |> ignore;

  verify_function init;
  Llvm.PassManager.run_function init the_fpm |> ignore;
  L.dump_module the_module;

  let init_fp = Llvm_executionengine.get_function_address "init_builtins" 
    (Foreign.funptr Ctypes.(void @-> returning void)) 
    the_execution_engine in
  init_fp ();
  ()



let codegen_repl (a: A.last) : unit = 
  let ta = Last.get_type a in
  let repl_function = L.declare_function "repl" (L.function_type (gen_type ta) [||]) the_module in
  let bb = L.append_block context "entry" repl_function in
  L.position_at_end bb builder;



  L.build_ret (codegen M.empty a) builder |> ignore;
  verify_function repl_function;
  
  L.dump_module the_module;

  Printf.printf "type : %s" (Type.show ta);

  (match ta with
  | Type.IntT -> 
    let repl_fp = Llvm_executionengine.get_function_address "repl" 
      (Foreign.funptr Ctypes.(void @-> returning int64_t)) 
      the_execution_engine in
    Printf.printf ", eval : %Ld\n" (repl_fp ())
  | _ -> 
    let repl_fp = Llvm_executionengine.get_function_address "repl" 
      (Foreign.funptr Ctypes.(void @-> returning void)) 
      the_execution_engine in
    repl_fp ();
    print_newline ()
  );

  L.delete_block bb;

  ()