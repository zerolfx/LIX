module L = Llvm
module A = Last


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
let closure_struct = L.struct_type context [| void_ptr_type ; void_ptr_type |]
let closure_ptr_type = L.pointer_type closure_struct


let llvm_prefix = "__llvm"

let declare_global name ty =
  let global = L.define_global name (L.const_null ty) the_module in
  L.set_linkage L.Linkage.Common global;
  global 

let gen_type = function
| Type.IntT -> int_type
| Type.BoolT -> bool_type
| Type.FunT _ -> closure_ptr_type
| t -> raise (Failure (Printf.sprintf "unsupported type %s" (Type.show t)))


let dump_value v = L.dump_value v; Core.fprintf stderr "\n"; flush stderr

let verify_and_optimize f : unit = 
  if not (Llvm_analysis.verify_function f) then (dump_value f; Llvm_analysis.assert_valid_function f);
  Llvm.PassManager.run_function f the_fpm |> ignore

