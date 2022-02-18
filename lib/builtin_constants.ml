module L = Llvm

type builtin = {
  name : string;
  internal_name : string;
  ty : Type.t;
  gen : L.llvalue list -> L.llbuilder -> L.llvalue;
}

let [@warning "-8"] builtins = [
  { name = "+"; 
    internal_name = "__builtin_add"; 
    ty = Type.FunctionT (Type.IntT, Type.FunctionT (Type.IntT, Type.IntT)); 
    gen = fun [x;y] -> L.build_add x y "add";
  };
  {
    name = "-";
    internal_name = "__builtin_sub";
    ty = Type.FunctionT (Type.IntT, Type.FunctionT (Type.IntT, Type.IntT));
    gen = fun [x;y] -> L.build_sub x y "sub";
  };
  {
    name = "*";
    internal_name = "__builtin_mul";
    ty = Type.FunctionT (Type.IntT, Type.FunctionT (Type.IntT, Type.IntT));
    gen = fun [x;y] -> L.build_mul x y "mul";
  };
  {
    name = "/";
    internal_name = "__builtin_div";
    ty = Type.FunctionT (Type.IntT, Type.FunctionT (Type.IntT, Type.IntT));
    gen = fun [x;y] -> L.build_sdiv x y "div";
  };
  {
    name = "%";
    internal_name = "__builtin_mod";
    ty = Type.FunctionT (Type.IntT, Type.FunctionT (Type.IntT, Type.IntT));
    gen = fun [x;y] -> L.build_srem x y "rem";
  };
  {
    name = "<";
    internal_name = "__builtin_lt";
    ty = Type.FunctionT (Type.IntT, Type.FunctionT (Type.IntT, Type.BoolT));
    gen = fun [x;y] -> L.build_icmp L.Icmp.Slt x y "lt";
  };
  {
    name = "<=";
    internal_name = "__builtin_le";
    ty = Type.FunctionT (Type.IntT, Type.FunctionT (Type.IntT, Type.BoolT));
    gen = fun [x;y] -> L.build_icmp L.Icmp.Sle x y "le";
  };
  {
    name = ">";
    internal_name = "__builtin_gt";
    ty = Type.FunctionT (Type.IntT, Type.FunctionT (Type.IntT, Type.BoolT));
    gen = fun [x;y] -> L.build_icmp L.Icmp.Sgt x y "gt";
  };
  {
    name = ">=";
    internal_name = "__builtin_ge";
    ty = Type.FunctionT (Type.IntT, Type.FunctionT (Type.IntT, Type.BoolT));
    gen = fun [x;y] -> L.build_icmp L.Icmp.Sge x y "ge";
  };
  {
    name = "==";
    internal_name = "__builtin_eq";
    ty = Type.FunctionT (Type.IntT, Type.FunctionT (Type.IntT, Type.BoolT));
    gen = fun [x;y] -> L.build_icmp L.Icmp.Eq x y "eq";
  };
  {
    name = "!=";
    internal_name = "__builtin_ne";
    ty = Type.FunctionT (Type.IntT, Type.FunctionT (Type.IntT, Type.BoolT));
    gen = fun [x;y] -> L.build_icmp L.Icmp.Ne x y "ne";
  };
  {
    name = "&&";
    internal_name = "__builtin_and";
    ty = Type.FunctionT (Type.BoolT, Type.FunctionT (Type.BoolT, Type.BoolT));
    gen = fun [x;y] -> L.build_and x y "and";
  };
  {
    name = "||";
    internal_name = "__builtin_or";
    ty = Type.FunctionT (Type.BoolT, Type.FunctionT (Type.BoolT, Type.BoolT));
    gen = fun [x;y] -> L.build_or x y "or";
  };
  {
    name = "not";
    internal_name = "__builtin_not";
    ty = Type.FunctionT (Type.BoolT, Type.BoolT);
    gen = fun [x] -> L.build_not x "not";
  };
]