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

type last = 
| Primitive of Type.primitive
| Var of Type.t_var
| Lambda of string * Type.t_var list * Type.t_var * last
| Application of Type.t * last * last
| Define of string * last
| If of last * last * last
[@@deriving show]


let rec get_type = function
| Primitive (Type.Int _) -> Type.IntT
| Primitive (Type.Bool _) -> Type.BoolT
| Var (_, t) -> t
| Lambda (_, _, (_, ta), e) -> Type.FunT (ta, get_type e)
| Application (t, _, _) -> t
| Define (_, e) -> get_type e
| If (_, e1, _) -> get_type e1


let convert_scheme = function
| Type.Scheme (fv, t) ->
  assert (List.length fv = 0);
  t

let [@warning "-8"] rec convert (env : Type.t M.t) (tast : Type.scheme Tast.tast) : last = match tast with
| Tast.Primitive p -> Primitive p
| Tast.Variable n -> 
  if String.starts_with ~prefix:"__builtin" n
  then Var (n, Tast.M.find n !Tast.globals) 
  else Var (n, M.find n env)
| Tast.Application (f, a) -> 
  let f' = convert env f in
  let a' = convert env a in
  let Type.FunT(_, r) = get_type f' in
  Application (r, f', a')
| Tast.Lambda (t, n, e) ->
  let t' = convert_scheme t in
  let Type.FunT (ta, _) = t' in
  let env' = M.add n ta env in
  let e' = convert env' e in
  let fv = free_vars tast in
  let tfv = List.map (fun n -> (n, M.find n env)) (fv |> S.to_seq |> List.of_seq) in
  Lambda (gen_name "lambda", tfv, (n, ta), e')
| Tast.If (c, e1, e2) -> If (convert env c, convert env e1, convert env e2)

let tast_to_last = convert M.empty
