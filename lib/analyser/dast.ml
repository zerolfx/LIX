type dast =
| Lambda of Type.var * dast
| TypeAnnotation of Type.t * dast
| Primitive of Type.primitive
| Application of dast * dast
| Variable of Type.var
| Define of Type.var * dast
| If of dast * dast * dast
[@@deriving show]


let rec desugar_ast (a: Ast.ast): dast = match a with
| Ast.Primitive p -> Primitive p
| Ast.TypeAnnotation (t, e) -> TypeAnnotation (t, desugar_ast e)

| Ast.Application (f, []) -> desugar_ast f
| Ast.Application (f, [arg]) -> Application (desugar_ast f, desugar_ast arg)
| Ast.Application (f, arg0 :: args) ->
  desugar_ast (Ast.Application (Ast.Application (f, [arg0]), args))

| Ast.Variable v -> (match List.find_opt (fun Builtin_constants.{ name; _ } -> String.equal name v ) Builtin_constants.builtins with
  | None -> Variable v
  | Some ({ internal_name; _}) -> Variable internal_name)

| Ast.Lambda ([], _) -> raise (Failure "lambda must has at least one argument")
| Ast.Lambda ([a], e) -> Lambda (a, desugar_ast e)
| Ast.Lambda (a0 :: args, e) -> desugar_ast (Ast.Lambda ([a0], Ast.Lambda (args, e)))

| Ast.Let (body, let_args) -> 
  let rec get_args_ast = (function
  | [] -> []
  | [arg] -> [snd arg]
  | (_, ast) :: args -> let l = Ast.Lambda(List.map fst args, ast) in
    let args_ast = get_args_ast args in
    Ast.Application(l, args_ast) :: args_ast) in
  let f = (match let_args with 
  | [] -> body
  | _ -> Ast.Lambda(List.map fst let_args, body)) in 
  desugar_ast (Ast.Application(f, get_args_ast let_args))

| Ast.Define (v, e) -> Define (v, desugar_ast e)
| Ast.If (c, e1, e2) -> If (desugar_ast c, desugar_ast e1, desugar_ast e2)

let ast_to_dast (a: Ast.ast): dast = desugar_ast a
