module S = Syntax

type var = string [@@deriving show]

type primitive =
| Int of int
| Bool of bool
[@@deriving show]

type ast =
| Lambda of var * ast
| TypeAnnotation of Type.t * ast
| Primitive of primitive
| Application of ast * ast list
| Variable of var
[@@deriving show]

exception Err of string

let rec gen_ast (c: S.code): ast = match c with
| S.IntLit i -> Primitive (Int i)
| S.BoolLit b -> Primitive (Bool b)
| S.Symbol s -> Variable s
| S.Form [S.Symbol ":"; t; e] -> TypeAnnotation (Type.parse_type t, gen_ast e)
| S.Form [S.Symbol "fn"; S.Symbol x; e] -> Lambda (x, gen_ast e)
| S.Form (f :: es) -> Application (gen_ast f, List.map gen_ast es)
| _ -> raise (Err "not supported")


let rec desugar_ast (a: ast): ast = match a with
| Application (f, []) -> f
| Application (f, [arg]) -> Application (desugar_ast f, [desugar_ast arg])
| Application (f, arg0 :: args) ->
  desugar_ast (Application (Application (f, [arg0]), args))
| Primitive _ | Variable _ -> a
| Lambda (x, e) -> Lambda (x, desugar_ast e)
| TypeAnnotation (t, e) -> TypeAnnotation (t, desugar_ast e)


let code_to_ast (c: S.code): ast = desugar_ast (gen_ast c)