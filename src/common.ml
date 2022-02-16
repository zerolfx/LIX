type typed_var = string * Type.t [@@deriving show, eq, ord]

let type_of_var (t : typed_var) = match t with | (_, t) -> t
let name_of_var (t : typed_var) = match t with | (n, _) -> n

let dump_value v = Llvm.dump_value v; Core.fprintf stderr "\n"; flush stderr