open Common

type hast =
| Primitive of Ast.primitive
| Var of typed_var
| Lambda of typed_var * hast
| Application of Type.t * hast * hast
[@@deriving show]

let rec gen_hast (dast: Dast.dast) = match dast with
| Dast.TypeAnnotation (t, a) -> (match a with
  | Dast.Primitive p -> Primitive p
  | Dast.TypeAnnotation _ -> raise (Failure "type_annotation for type_annotation is not allowed")
  | Dast.Variable s -> Var (s, t)
  | Dast.Application (v, arg) -> Application (t, gen_hast v, gen_hast arg)
  | Dast.Lambda (v, e) -> (match t with
    | Type.FunctionT (tv, te) -> Lambda ((v, tv), gen_hast (Dast.TypeAnnotation (te, e)))
    | _ -> raise (Failure "lambda type mismatch")
    )
  )
| Dast.Variable s -> Var (s, Type.DummyT)
| Dast.Application (v, arg) -> Application (Type.DummyT, gen_hast v, gen_hast arg)
| Dast.Primitive p -> Primitive p
| _ -> raise (Failure (Core.sprintf "unexpected ast node: %s" (Dast.show_dast dast)))

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
    | None -> raise (Failure (Core.sprintf "unknown variable: %s" name))


let rec get_type = function
| Primitive (Ast.Int _) -> Type.IntT
| Primitive (Ast.Bool _) -> Type.BoolT
| Var (_, t) -> t
| Lambda ((_, t), e) -> Type.FunctionT (t, get_type e)
| Application (t, _, _) -> t

let resolve_app (f_type : Type.t) (arg_type : Type.t) : Type.t = match f_type with
| Type.FunctionT (arg_t, expr_t) ->
  if arg_t == arg_type then expr_t else raise (Failure (
    Core.sprintf "function arg type mismatch: %s %s" (Type.show f_type) (Type.show arg_type)))
| _ -> raise (Failure (Core.sprintf "function type expected: %s" (Type.show f_type)))

let rec type_hast (table: typed_var list) (h: hast) : hast = match h with
| Primitive _ -> h
| Var (name, Type.DummyT) -> Var (lookup name table)
| Lambda (v, e) -> Lambda (v, type_hast (v :: table) e)
| Application (t, f, arg) -> (
  let typed_f = type_hast table f in
  let typed_arg = type_hast table arg in
  let arg_type = get_type typed_arg in
  let app_type = resolve_app (get_type typed_f) arg_type in
  if t != Type.DummyT then assert (t == app_type);
  Application (app_type, typed_f, typed_arg)
)
| _ -> raise (Failure (Core.sprintf "cannot type %s" (show_hast h)))


let rec check_all_typed = function
| Var (_, Type.DummyT) -> false
| Lambda (v, e) -> check_all_typed (Var v) && check_all_typed e
| Application (Type.DummyT, _, _) -> false
| Application (_, f, arg) -> check_all_typed f && check_all_typed arg
| _ -> true


let ast_to_hast (a : Ast.ast) : hast =
  let h = a |> Dast.ast_to_dast |> gen_hast |> type_hast [] in
  assert (check_all_typed h);
  h
