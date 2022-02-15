type var = string [@@deriving show]

type dast =
| Lambda of var * dast
| TypeAnnotation of Type.t * dast
| Primitive of Ast.primitive
| Application of dast * dast
| Variable of var
[@@deriving show]


let rec desugar_ast (a: Ast.ast): dast = match a with
| Ast.Application (f, []) -> desugar_ast f
| Ast.Application (f, [arg]) -> Application (desugar_ast f, desugar_ast arg)
| Ast.Application (f, arg0 :: args) ->
  desugar_ast (Ast.Application (Ast.Application (f, [arg0]), args))
| Ast.Primitive p -> Primitive p
| Ast.Variable v -> Variable v
| Ast.Lambda ([], _) -> raise (Failure "lambda must has at least one argument")
| Ast.Lambda ([a], e) -> Lambda (a, desugar_ast e)
| Ast.Lambda (a0 :: args, e) -> desugar_ast (Ast.Lambda ([a0], Ast.Lambda (args, e)))
| Ast.TypeAnnotation (t, e) -> TypeAnnotation (t, desugar_ast e)

let ast_to_dast (a: Ast.ast): dast = desugar_ast a