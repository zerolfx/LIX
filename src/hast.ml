type typed_var = string * Type.t [@@deriving show]

type hast =
| Primitive of Ast.primitive
| Var of typed_var
| Lambda of typed_var * hast
| Application of Type.t * hast * hast list
[@@deriving show]

exception Err
exception Error of string

let rec gen_hast = function
| Ast.TypeAnnotation (t, a) -> (match a with
  | Ast.Primitive p -> Primitive p
  | Ast.TypeAnnotation _ -> raise Err
  | Ast.Variable s -> Var (s, t)
  | Ast.Application (v, l) -> Application (t, gen_hast v, List.map gen_hast l)
  | Ast.Lambda (v, e) -> (match t with
    | Type.FunctionT (tv, te) -> Lambda ((v, tv), gen_hast (Ast.TypeAnnotation (te, e)))
    | _ -> raise Err
    )
  )
| Ast.Variable s -> Var (s, Type.DummyT)
| Ast.Application (v, l) -> Application (Type.DummyT, gen_hast v, List.map gen_hast l)
| Ast.Primitive p -> Primitive p
| _ -> raise Err

module M = Map.Make (String)
let globals = [
  ("+", Type.FunctionT (Type.IntT, Type.FunctionT (Type.IntT, Type.IntT)));
  ("-", Type.FunctionT (Type.IntT, Type.FunctionT (Type.IntT, Type.IntT)));
  ("*", Type.FunctionT (Type.IntT, Type.FunctionT (Type.IntT, Type.IntT)));
  ("/", Type.FunctionT (Type.IntT, Type.FunctionT (Type.IntT, Type.IntT)));
  ("<", Type.FunctionT (Type.IntT, Type.FunctionT (Type.IntT, Type.BoolT)));
  ("<=", Type.FunctionT (Type.IntT, Type.FunctionT (Type.IntT, Type.BoolT)));
  (">", Type.FunctionT (Type.IntT, Type.FunctionT (Type.IntT, Type.BoolT)));
  (">=", Type.FunctionT (Type.IntT, Type.FunctionT (Type.IntT, Type.BoolT)));
  ("=", Type.FunctionT (Type.IntT, Type.FunctionT (Type.IntT, Type.BoolT)));
  ("not", Type.FunctionT (Type.BoolT, Type.BoolT));
  ("and", Type.FunctionT (Type.BoolT, Type.FunctionT (Type.BoolT, Type.BoolT)));
  ("or", Type.FunctionT (Type.BoolT, Type.FunctionT (Type.BoolT, Type.BoolT)))
] |> List.to_seq |> M.of_seq

let rec lookup_local (name : string) (table : typed_var list) : typed_var option =
  match table with
  | [] -> None
  | (v, _) as tv :: rest ->
    if v = name then Some tv else lookup_local name rest

let lookup (name : string) (table : typed_var list) : typed_var =
  match lookup_local name table with
  | Some tv -> tv
  | None -> match M.find_opt name globals with
    | Some tv -> (name, tv)
    | None -> raise Err


let rec get_type = function
| Primitive (Ast.Int _) -> Type.IntT
| Primitive (Ast.Bool _) -> Type.BoolT
| Var (_, t) -> t
| Lambda ((_, t), e) -> Type.FunctionT (t, get_type e)
| Application (t, _, _) -> t

let rec resolve_app (ft : Type.t) (ts : Type.t list) : Type.t = match ft, ts with
| _, [] -> ft
| Type.FunctionT (tv, te), ta :: res ->
  if tv == ta then resolve_app te res else raise Err
| _, _ -> raise Err

let rec type_hast (table: typed_var list) (h: hast) : hast = match h with
| Primitive _ -> h
| Var (name, Type.DummyT) -> Var (lookup name table)
| Lambda (v, e) -> Lambda (v, type_hast (v :: table) e)
| Application (t, v, es) -> (
  let tv = type_hast table v in
  let tes = List.map (type_hast table) es in
  let ts = List.map get_type tes in
  let ta = resolve_app (get_type tv) ts in
  if t != Type.DummyT then assert (t == ta);
  Application (ta, tv, tes)
)
| _ -> raise (Error (Core.sprintf "cannot match %s" (show_hast h)))


let rec check_all_typed = function
| Var (_, Type.DummyT) -> false
| Lambda (v, e) -> check_all_typed (Var v) && check_all_typed e
| Application (Type.DummyT, _, _) -> false
| Application (_, f, vs) -> check_all_typed f && List.for_all check_all_typed vs
| _ -> true


let ast_to_hast (a : Ast.ast) : hast =
  let h = a |> gen_hast |> type_hast [] in
  assert (check_all_typed h);
  h
