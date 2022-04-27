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
| Ast.Let (body, let_args) ->
  let fold_args_to_dast (prev_args : dast list) ((name : Type.var), (ast : Ast.ast)) =
    (let arg_dast = Application (desugar_ast ast, prev_args) in
    prev_args@[arg_dast], arg_dast) in
  let dast_tuple_list = List.fold_left_map fold_args_to_dast [] let_args in (* list(list(dast)*dast) *)
  Application (desugar_ast body, List.map (fun (_, arg) -> arg) dast_tuple_list)

| Ast.Define (v, e) -> Define (v, desugar_ast e)
| Ast.If (c, e1, e2) -> If (desugar_ast c, desugar_ast e1, desugar_ast e2)

let ast_to_dast (a: Ast.ast): dast = desugar_ast a
