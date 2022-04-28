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

| Ast.Let (body, []) -> desugar_ast body
| Ast.Let (body, let_arg0 :: let_args) ->
  let (arg0_name, arg0_ast) = let_arg0 in 
  let arg0_dast = desugar_ast arg0_ast in
  let fold_args (f : dast) (args : dast list) = List.fold_left (fun body arg -> Application(body, arg)) f args in
  let fold_let_args ((arg_names : Type.var list), (args_dast : dast list)) ((name : Type.var), (arg_ast : Ast.ast)) = 
    (let arg_body_dast = desugar_ast (Ast.Lambda(arg_names, arg_ast)) in 
    name :: arg_names, (fold_args arg_body_dast args_dast) :: args_dast) in
  let (all_names, all_args_dast) = List.fold_left fold_let_args ([arg0_name], [arg0_dast]) let_args in
  fold_args (desugar_ast (Ast.Lambda(all_names, body))) all_args_dast

| Ast.Define (v, e) -> Define (v, desugar_ast e)
| Ast.If (c, e1, e2) -> If (desugar_ast c, desugar_ast e1, desugar_ast e2)

let ast_to_dast (a: Ast.ast): dast = desugar_ast a
