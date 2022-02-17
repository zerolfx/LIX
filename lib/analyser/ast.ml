module S = Syntax

type var = string [@@deriving show]

type primitive =
| Int of int
| Bool of bool
[@@deriving show]


type ast =
| Lambda of var list * ast
| TypeAnnotation of Type.t * ast
| Primitive of primitive
| Application of ast * ast list
| Variable of var
| Define of var * ast
[@@deriving show]

exception Err of string

let parse_arg = function
| S.Symbol s -> s
| _ -> raise (Failure "parse_args")

let rec gen_ast (c: S.code): ast = match c with
| S.IntLit i -> Primitive (Int i)
| S.BoolLit b -> Primitive (Bool b)
| S.Symbol s -> Variable s
| S.Form [S.Symbol ":"; t; e] -> TypeAnnotation (Type.parse_type t, gen_ast e)
| S.Form [S.Symbol "def"; S.Symbol name; e] -> Define (name, (gen_ast e))
| S.Form [S.Symbol "fn"; S.Form args; e] -> Lambda (List.map parse_arg args, gen_ast e)
| S.Form [S.Symbol "fn"; S.Symbol arg; e] -> Lambda ([arg], gen_ast e)
| S.Form (f :: es) -> Application (gen_ast f, List.map gen_ast es)
| _ -> raise (Err "not supported")


let code_to_ast (c: S.code): ast = gen_ast c