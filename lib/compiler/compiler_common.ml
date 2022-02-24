module L = Llvm

module M = Map.Make(String)

let context = L.global_context ()
let the_module = L.create_module context "LIX"


let the_fpm = 
  let fpm = Llvm.PassManager.create_function the_module in
  Llvm_scalar_opts.add_memory_to_register_promotion fpm;
  Llvm_scalar_opts.add_instruction_combination fpm;
  Llvm_scalar_opts.add_reassociation fpm;
  Llvm_scalar_opts.add_gvn fpm;
  Llvm_scalar_opts.add_cfg_simplification fpm;
  Llvm.PassManager.initialize fpm |> ignore;
  fpm


let builder = L.builder context


let int_type = L.i64_type context
let bool_type = L.i1_type context
let void_type = L.void_type context
let void_ptr_type = L.pointer_type void_type
(* closure { void *function; struct env *env; } *)

let function_type = L.function_type void_ptr_type [| void_ptr_type; void_ptr_type |]
let function_ptr_type = L.pointer_type function_type
let closure_struct = L.struct_type context [| function_ptr_type ; void_ptr_type |]
let closure_ptr_type = L.pointer_type closure_struct


let build_malloc_ptr (t : L.lltype) : L.llvalue = 
  L.build_malloc t "malloc" builder


let store_void_ptr (v : L.llvalue) = 
  let ptr = build_malloc_ptr (L.type_of v) in
  L.build_store v ptr builder |> ignore;
  L.build_bitcast ptr void_ptr_type "void_ptr"

let load_void_ptr (t : L.lltype) (ptr : L.llvalue) (builder : L.llbuilder) : L.llvalue = 
  L.build_load (L.build_bitcast ptr (L.pointer_type t) "cast" builder) "load" builder


let llvm_prefix = "__llvm"

let declare_global name ty =
  let global = L.define_global name (L.const_null ty) the_module in
  L.set_linkage L.Linkage.Common global;
  global 


let dump_value v = L.dump_value v; Core.fprintf stderr "\n"; flush stderr

let verify_and_optimize f : unit = 
  if not (Llvm_analysis.verify_function f) then (dump_value f; Llvm_analysis.assert_valid_function f);
  Llvm.PassManager.run_function f the_fpm |> ignore

