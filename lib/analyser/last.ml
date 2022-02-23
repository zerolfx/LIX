open Common
module S = Set.Make (String)

let rec free_vars = function
| Tast.Primitive _ -> S.empty
| Tast.Variable v -> (
  if Tast.M.mem v !Tast.globals then S.empty
  else S.singleton v)
| Tast.Application (f, a) -> S.union (free_vars f) (free_vars a)
| Tast.Lambda (_, a, e) -> S.remove a (free_vars e)
| Tast.If (c, e1, e2) -> S.union (free_vars c) (S.union (free_vars e1) (free_vars e2))
| Tast.Define (n, e) -> S.remove n (free_vars e)

type last = 
| Primitive of Type.primitive
| Variable of Type.var
| Lambda of string * Type.var list * Type.var * last
| Application of last * last
| Define of string * last
| If of last * last * last
[@@deriving show]



let convert_scheme = function
| Type.Scheme (fv, t) ->
  assert (List.length fv = 0);
  t

let rec convert (env : S.t) (tast : Type.scheme Tast.tast) : last = match tast with
| Tast.Primitive p -> Primitive p
| Tast.Variable n -> Variable n
| Tast.Application (f, a) -> Application (convert env f, convert env a)
| Tast.Lambda (_, n, e) ->
  let env' = S.add n env in
  let e' = convert env' e in
  let fv = free_vars tast in
  Lambda (gen_name "lambda", fv |> S.to_seq |> List.of_seq, n, e')
| Tast.If (c, e1, e2) -> If (convert env c, convert env e1, convert env e2)
| Define (n, e) -> Define (n, convert env e)

let tast_to_last = convert S.empty
