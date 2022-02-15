module H = Hast
module L = Llvm

let context = L.global_context ()
let int_type = L.i64_type context
let bool_type = L.i1_type context


let codegen_primitive = function
| Ast.Int i -> L.const_int int_type i
| Ast.Bool b -> L.const_int bool_type (if b then 1 else 0)
