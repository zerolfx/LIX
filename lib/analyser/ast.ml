module S = Syntax

type ast =
| Lambda of Type.var list * ast
| TypeAnnotation of Type.t * ast
| Primitive of Type.primitive
| Application of ast * ast list
| Variable of Type.var
| Define of Type.var * ast
| If of ast * ast * ast
| Let of ast * (Type.var * ast) list (* let *)
[@@deriving show]

let rec parse_type (c: S.code): Type.t = match c with
| Symbol "Int" -> IntT
| Symbol "Bool" ->  BoolT
| Form (Symbol "->" :: cs) -> parse_function_type cs
| _ -> raise (Failure "parse_type")
and parse_function_type (cs: S.code list) = match cs with
| [c1; c2] -> FunT (parse_type c1, parse_type c2)
| c1 :: cs -> FunT (parse_type c1, parse_function_type cs)
| _ -> raise (Failure "malformed function type")

let parse_arg = function
| S.Symbol s -> s
| _ -> raise (Failure "parse_args")

let rec gen_ast (c: S.code): ast = match c with
| S.IntLit i -> Primitive (Int i)
| S.BoolLit b -> Primitive (Bool b)
| S.Symbol s -> Variable s
| S.Form [S.Symbol ":"; t; e] -> TypeAnnotation (parse_type t, gen_ast e)
| S.Form [S.Symbol "def"; S.Symbol name; e] -> Define (name, (gen_ast e))
| S.Form [S.Symbol "fn"; S.Form args; e] -> Lambda (List.map parse_arg args, gen_ast e)
| S.Form [S.Symbol "fn"; S.Symbol arg; e] -> Lambda ([arg], gen_ast e)
| S.Form [S.Symbol "if"; e1; e2; e3] -> If (gen_ast e1, gen_ast e2, gen_ast e3)
| S.Form [S.Symbol "let"; S.Form let_args; body] ->
  let parse_let_args = (function
  | S.Form [S.Symbol name; e] -> (name, gen_ast e)
  | _ -> raise (Failure "parse_let_args")) in
  Let (gen_ast body, List.map parse_let_args let_args) (* let *)
| S.Form (f :: es) -> Application (gen_ast f, List.map gen_ast es)
| S.Form [] -> raise (Failure "empty form")


let code_to_ast (c: S.code): ast = gen_ast c
