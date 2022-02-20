module S = Syntax

type t =
| IntT
| BoolT
| FunctionT of t * t
| DummyT
[@@deriving show, eq, ord]


let rec parse_type (c: S.code): t = match c with
| S.Symbol "Int" -> IntT
| S.Symbol "Bool" -> BoolT
| S.Form (S.Symbol "->" :: cs) -> parse_function_type cs
| _ -> raise (Failure "parse_type")
and parse_function_type (cs: S.code list) = match cs with
| [c1; c2] -> FunctionT (parse_type c1, parse_type c2)
| c1 :: cs -> FunctionT (parse_type c1, parse_function_type cs)
| _ -> raise (Failure "malformed function type")