module L = Llvm
open Compiler_common

type builtin = {
  name : string;
  internal_name : string;
  ty : Type.t;
  gen : L.llvalue list -> L.llbuilder -> L.llvalue;
}

let load_int = load_void_ptr int_type
let load_bool = load_void_ptr bool_type

let [@warning "-8"] builtins = [
  { name = "+"; 
    internal_name = "__builtin_add"; 
    ty = Type.FunT (Type.IntT, Type.FunT (Type.IntT, Type.IntT)); 
    gen = fun [x;y] builder -> store_void_ptr (L.build_add (load_int x builder) (load_int y builder) "add" builder) builder;
  };
  {
    name = "-";
    internal_name = "__builtin_sub";
    ty = Type.FunT (Type.IntT, Type.FunT (Type.IntT, Type.IntT));
    gen = fun [x;y] builder -> store_void_ptr (L.build_sub (load_int x builder) (load_int y builder) "sub" builder) builder;
  };
  {
    name = "*";
    internal_name = "__builtin_mul";
    ty = Type.FunT (Type.IntT, Type.FunT (Type.IntT, Type.IntT));
    gen = fun [x;y] builder -> store_void_ptr (L.build_mul (load_int x builder) (load_int y builder) "mul" builder) builder;
  };
  {
    name = "/";
    internal_name = "__builtin_div";
    ty = Type.FunT (Type.IntT, Type.FunT (Type.IntT, Type.IntT));
    gen = fun [x;y] builder -> store_void_ptr (L.build_sdiv (load_int x builder) (load_int y builder) "div" builder) builder;
  };
  {
    name = "%";
    internal_name = "__builtin_mod";
    ty = Type.FunT (Type.IntT, Type.FunT (Type.IntT, Type.IntT));
    gen = fun [x;y] builder -> store_void_ptr (L.build_srem (load_int x builder) (load_int y builder) "mod" builder) builder;
  };
  {
    name = "<";
    internal_name = "__builtin_lt";
    ty = Type.FunT (Type.IntT, Type.FunT (Type.IntT, Type.BoolT));
    gen = fun [x;y] builder -> store_void_ptr (L.build_icmp L.Icmp.Slt (load_int x builder) (load_int y builder) "lt" builder) builder;
  };
  {
    name = "<=";
    internal_name = "__builtin_le";
    ty = Type.FunT (Type.IntT, Type.FunT (Type.IntT, Type.BoolT));
    gen = fun [x;y] builder -> store_void_ptr (L.build_icmp L.Icmp.Sle (load_int x builder) (load_int y builder) "le" builder) builder;
  };
  {
    name = ">";
    internal_name = "__builtin_gt";
    ty = Type.FunT (Type.IntT, Type.FunT (Type.IntT, Type.BoolT));
    gen = fun [x;y] builder -> store_void_ptr (L.build_icmp L.Icmp.Sgt (load_int x builder) (load_int y builder) "gt" builder) builder;
  };
  {
    name = ">=";
    internal_name = "__builtin_ge";
    ty = Type.FunT (Type.IntT, Type.FunT (Type.IntT, Type.BoolT));
    gen = fun [x;y] builder -> store_void_ptr (L.build_icmp L.Icmp.Sge (load_int x builder) (load_int y builder) "ge" builder) builder;
  };
  {
    name = "==";
    internal_name = "__builtin_eq";
    ty = Type.FunT (Type.IntT, Type.FunT (Type.IntT, Type.BoolT));
    gen = fun [x;y] builder -> store_void_ptr (L.build_icmp L.Icmp.Eq (load_int x builder) (load_int y builder) "eq" builder) builder;
  };
  {
    name = "!=";
    internal_name = "__builtin_ne";
    ty = Type.FunT (Type.IntT, Type.FunT (Type.IntT, Type.BoolT));
    gen = fun [x;y] builder -> store_void_ptr (L.build_icmp L.Icmp.Ne (load_int x builder) (load_int y builder) "ne" builder) builder;
  };
  {
    name = "&&";
    internal_name = "__builtin_and";
    ty = Type.FunT (Type.BoolT, Type.FunT (Type.BoolT, Type.BoolT));
    gen = fun [x;y] builder -> store_void_ptr (L.build_and (load_bool x builder) (load_bool y builder) "and" builder) builder;
  };
  {
    name = "||";
    internal_name = "__builtin_or";
    ty = Type.FunT (Type.BoolT, Type.FunT (Type.BoolT, Type.BoolT));
    gen = fun [x;y] builder -> store_void_ptr (L.build_or (load_bool x builder) (load_bool y builder) "or" builder) builder;
  };
  {
    name = "not";
    internal_name = "__builtin_not";
    ty = Type.FunT (Type.BoolT, Type.BoolT);
    gen = fun [x] builder -> store_void_ptr (L.build_not (load_bool x builder) "not" builder) builder;
  };
]