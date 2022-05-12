module S = Syntax

type ast =
| Lambda of Type.var list * ast
| TypeAnnotation of Type.t * ast
| Primitive of Type.primitive
| Application of ast * ast list
| Variable of Type.var
| Define of Type.var * ast
| If of ast * ast * ast
| Let of ast * (Type.var * ast) list
| Custom of Type.constructor * ast list
| TypeDefinition of string * Type.var list * Type.constructor_definition list
| Case of ast * (Type.pattern * ast) list
[@@deriving show]

let rec parse_type (c: S.code): Type.t = match c with
| Symbol "Int" -> IntT
| Symbol "Bool" ->  BoolT
| Symbol s -> VarT s
| Form (Symbol "->" :: cs) -> parse_function_type cs
| Form (S.Symbol name :: cs) -> CustomT (name, List.map parse_type cs)
| _ -> raise (Failure "parse_type")
and parse_function_type (cs: S.code list) = match cs with
| [c1; c2] -> FunT (parse_type c1, parse_type c2)
| c1 :: cs -> FunT (parse_type c1, parse_function_type cs)
| _ -> raise (Failure "malformed function type")

let parse_name = function
| S.Symbol s -> s
| _ -> raise (Failure "parse_args")

let parse_constructor_definition = function
| S.CForm (name, ts) -> Type.ConstructorDefinition (name, List.map parse_type ts)
| _ -> raise (Failure "not a valid constructor definition")

let rec parse_pattern = function
| S.Symbol "_" -> Type.WildcardPattern
| S.Symbol s -> Type.NamePattern s
| S.CForm (name, args) -> Type.ConstructorPattern (name, List.map parse_pattern args)
| S.IntLit i -> Type.LiteralPattern (Int i)
| S.BoolLit b -> Type.LiteralPattern (Bool b)
| _ -> raise (Failure "not a valid pattern")

let rec parse_case = function
| [] -> []
| a :: b :: cs -> (parse_pattern a, gen_ast b) :: parse_case cs
| _ -> raise (Failure "cases are not in pairs")
and gen_ast (c: S.code): ast = match c with
| S.IntLit i -> Primitive (Int i)
| S.BoolLit b -> Primitive (Bool b)
| S.Symbol s -> Variable s
| S.Form [S.Symbol ":"; t; e] -> TypeAnnotation (parse_type t, gen_ast e)
| S.Form [S.Symbol "def"; S.Symbol name; e] -> Define (name, (gen_ast e))
| S.Form [S.Symbol "fn"; S.Form args; e] -> Lambda (List.map parse_name args, gen_ast e)
| S.Form [S.Symbol "fn"; S.Symbol arg; e] -> Lambda ([arg], gen_ast e)
| S.Form [S.Symbol "if"; e1; e2; e3] -> If (gen_ast e1, gen_ast e2, gen_ast e3)

| S.Form [S.Symbol "let"; S.Form let_args; body] ->
  let parse_let_args = function
  | S.Form [S.Symbol name; e] -> (name, gen_ast e)
  | _ -> raise (Failure "parse_let_args") in
  Let (gen_ast body, List.map parse_let_args let_args) (* let *)

| S.CForm (name, args) -> Custom (name, List.map gen_ast args)
| S.Form (S.Symbol "type:" :: (S.Form (S.Symbol t_name :: t_vars)) :: defs) ->
  TypeDefinition (t_name, List.map parse_name t_vars, List.map parse_constructor_definition defs)
| S.Form (S.Symbol "case" :: e :: cases) -> Case (gen_ast e, parse_case cases)

| S.Form (f :: es) -> Application (gen_ast f, List.map gen_ast es)

| S.Form [] -> raise (Failure "empty form")


let code_to_ast (c: S.code): ast = gen_ast c
