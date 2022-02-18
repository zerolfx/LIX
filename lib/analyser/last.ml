open Common

module VS = Set.Make(
  struct
    let compare = compare_typed_var
    type t = typed_var
  end
)

let rec free_vars = function
| Hast.Primitive _ -> VS.empty
| Hast.Var v -> (
  match Hast.lookup_opt (name_of_var v) [] with
  | Some _ -> VS.empty
  | None -> VS.singleton v)
| Hast.Application (_, f, a) -> VS.union (free_vars f) (free_vars a)
| Hast.Lambda (v, a) -> VS.remove v (free_vars a)
| Hast.Define _ -> raise (Failure "define must be at top level")
| Hast.If (c, e1, e2) -> VS.union (free_vars c) (VS.union (free_vars e1) (free_vars e2))

type last = 
| Primitive of Ast.primitive
| Var of typed_var
| Lambda of string * typed_var list * typed_var * last
| Application of Type.t * last * last
| Define of string * last
| If of last * last * last
[@@deriving show]


let rec get_type = function
| Primitive (Ast.Int _) -> Type.IntT
| Primitive (Ast.Bool _) -> Type.BoolT
| Var (_, t) -> t
| Lambda (_, _, (_, t), e) -> Type.FunctionT (t, get_type e)
| Application (t, _, _) -> t
| Define (_, e) -> get_type e
| If (_, e1, e2) -> assert (Type.equal (get_type e1) (get_type e2)); get_type e1

let rec hast_to_last (h : Hast.hast) : last = match h with
| Hast.Primitive p -> Primitive p
| Hast.Var v -> Var v
| Hast.Application (t, f, a) -> Application (t, hast_to_last f, hast_to_last a)
| Hast.Lambda (v, e) -> Lambda (gen_name "lambda", free_vars h |> VS.to_seq |> List.of_seq, v, hast_to_last e)
| Hast.Define (v, e) -> Define (v, hast_to_last e)
| Hast.If (c, e1, e2) -> If (hast_to_last c, hast_to_last e1, hast_to_last e2)