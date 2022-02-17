type typed_var = string * Type.t [@@deriving show, eq, ord]

let type_of_var (t : typed_var) = match t with | (_, t) -> t
let name_of_var (t : typed_var) = match t with | (n, _) -> n

let dump_value v = Llvm.dump_value v; Core.fprintf stderr "\n"; flush stderr


module M = Map.Make (String)
let counter = ref M.empty
let gen_name prefix =
  let c = match M.find_opt prefix !counter with
  | None -> 0
  | Some x -> x + 1 in
  counter := M.add prefix c !counter;
  prefix ^ "_" ^Int.to_string c