open Common

module VS = Set.Make(
  struct
    let compare = compare_typed_var
    type t = typed_var
  end
)


let lambda_count = ref 0
let gen_lambda_name () =
  lambda_count := !lambda_count + 1;
"__lambda_" ^ string_of_int !lambda_count

let rec free_vars = function
| Hast.Primitive _ -> VS.empty
| Hast.Var v -> VS.singleton v
| Hast.Application (_, f, a) -> VS.union (free_vars f) (free_vars a)
| Hast.Lambda (v, a) -> VS.remove v (free_vars a)

type last = 
| Primitive of Ast.primitive
| Var of typed_var
| Lambda of string * typed_var list * typed_var * last
| Application of Type.t * last * last
[@@deriving show]


let rec get_type = function
| Primitive (Ast.Int _) -> Type.IntT
| Primitive (Ast.Bool _) -> Type.BoolT
| Var (_, t) -> t
| Lambda (_, _, (_, t), e) -> Type.FunctionT (t, get_type e)
| Application (t, _, _) -> t

let rec hast_to_last (h : Hast.hast) : last = match h with
| Hast.Primitive p -> Primitive p
| Hast.Var v -> Var v
| Hast.Application (t, f, a) -> Application (t, hast_to_last f, hast_to_last a)
| Hast.Lambda (v, e) -> Lambda (gen_lambda_name(), free_vars h |> VS.to_seq |> List.of_seq, v, hast_to_last e)