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


let malloc_f = L.declare_function "malloc" (L.function_type void_ptr_type [| int_type |]) the_module

let build_malloc (size : L.llvalue) : L.llvalue = 
  L.build_call malloc_f [| size |] "malloc" builder

let build_malloc_ptr (t : L.lltype) : L.llvalue = 
  L.build_bitcast (L.size_of t) (L.pointer_type t) "malloc_ptr" builder


let build_struct_field_ptr (ptr : L.llvalue) (field : int) : L.llvalue = 
  L.build_struct_gep ptr field "struct_field_ptr" builder


let codegen_primitive = function
| Ast.Int i -> L.const_int int_type i
| Ast.Bool b -> L.const_int bool_type (if b then 1 else 0)


let gen_type = function
| Type.IntT -> int_type
| Type.BoolT -> bool_type
| Type.FunctionT _ -> closure_ptr_type
| t -> raise (Failure (Printf.sprintf "unsupported type %s" (Type.show t)))

let gen_env_type (vars : typed_var list) : L.lltype =
  L.struct_type context (List.map (Core.Fn.compose gen_type type_of_var) vars |> Array.of_list)
  

(* let gen_param (v) *)

module M = Map.Make(String)

let rec codegen (var_table : L.llvalue M.t) (a: A.last) : L.llvalue = match a with
| A.Primitive p -> codegen_primitive p
| A.Var (name, _) -> 
  if String.starts_with ~prefix:"__llvm" name then
    codegen_builtin var_table name
  else M.find name var_table
| A.Lambda (name, free_vars, arg, body) -> 
  let env_type = gen_env_type free_vars in
  let env_ptr = build_malloc_ptr env_type in
  List.mapi (fun i v ->
    L.build_store (codegen var_table (A.Var v)) (build_struct_field_ptr env_ptr i)) free_vars |> ignore;

  let f_llvm_type = L.function_type (Last.get_type body |> gen_type) [| type_of_var arg |> gen_type ; void_ptr_type |]  in
  let f = L.declare_function name f_llvm_type the_module in

  let closure_ptr = build_malloc_ptr closure_struct in

  
  List.mapi (fun i (n, _) ->
    L.build_store (M.find n var_table) (build_struct_field_ptr env_ptr i)) free_vars |> ignore;

  L.build_store 
    (L.build_bitcast f void_ptr_type "f_ptr" builder) 
    (build_malloc_ptr void_ptr_type) builder |> ignore;
  L.build_store
    (L.build_bitcast env_ptr void_ptr_type "env_ptr" builder)
    (L.build_struct_gep closure_ptr 1 "env_ptr" builder) builder |> ignore;
  
  
  let block = L.append_block context "entry" f in
  L.position_at_end block builder;


  let f_env_ptr = L.build_bitcast (L.param f 1) (L.pointer_type env_type) "f_env_ptr" builder in

  let table = 
    (name_of_var arg, L.param f 0) ::
    List.mapi (fun i v -> 
      (name_of_var v, L.build_load (build_struct_field_ptr f_env_ptr i) "env_arg"  builder)
    ) free_vars |>  List.to_seq |> M.of_seq  in

  codegen table body |> ignore;
  (* L.build_ret (codegen table body) builder |> ignore; *)
  
  closure_ptr
| A.Application (_, f, arg) -> (match Last.get_type f with
  | Type.FunctionT (f_type, arg_type) ->
    let closure_ptr = codegen var_table f in
    let f_void_ptr = L.build_struct_gep closure_ptr 0 "f_void_ptr" builder in
    let env_void_ptr = L.build_struct_gep closure_ptr 1 "env_void_ptr" builder in
    let f_llvm_type = L.function_type (gen_type f_type) [| (gen_type arg_type) ; void_ptr_type |] in
    let f_ptr = L.build_bitcast f_void_ptr f_llvm_type "f_ptr" builder in
    L.build_call f_ptr [| codegen var_table arg ; env_void_ptr |] "call" builder
  | _ -> raise (Failure "invalid function type")
)
and codegen_builtin (var_table : L.llvalue M.t) (name : string) : L.llvalue = match name with
| "__llvm__add" -> L.build_add (M.find "a" var_table) (M.find "b" var_table) "add" builder
| _ -> raise (Failure "unsupported builtin")


let builtin = 
  [("__llvm__add", 
    A.Lambda ("__builtin__add2", [], ("a", Type.IntT), 
      A.Lambda ("__builtin__add1", [("a", Type.IntT)], ("b", Type.IntT), 
        A.Var ("__llvm__add", Type.IntT))))]


        
let builtin_table =   
  let init = L.declare_function "init" (L.function_type void_ptr_type [||]) the_module in
  let bb = L.append_block context "entry" init in
  L.position_at_end bb builder;
  let table = builtin |> List.map (fun (n, c) -> (n, codegen M.empty c)) |> List.to_seq |> M.of_seq in
  L.build_ret_void builder |> ignore;

  (* Llvm_analysis.assert_valid_function init; *)

  (* let init_fp = Llvm_executionengine.get_function_address "init" 
    (Foreign.funptr Ctypes.(void @-> returning void)) 
    the_execution_engine in
  init_fp (); *)

  table



let codegen_with_builtins (a: A.last) : L.llvalue = 
  let repl_function = L.declare_function "repl" (L.function_type (a |> Last.get_type |> gen_type) [||]) the_module in
  let bb = L.append_block context "entry" repl_function in
  L.position_at_end bb builder;
  L.build_ret (codegen builtin_table a) builder