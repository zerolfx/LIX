module S = Syntax

type t =
| IntT
| BoolT
| SumT of t * t
| ProductT of t * t
| FunctionT of t * t
| VarT of t
| DummyT
[@@deriving show]


let rec parse_type (c: S.code): t = match c with
| S.Symbol "Int" -> IntT
| S.Symbol "Bool" -> BoolT
| S.Form (S.Symbol "->" :: cs) -> parse_function_type cs
| _ -> raise (Failure "parse_type")
and parse_function_type (cs: S.code list) = match cs with
| [c1; c2] -> FunctionT (parse_type c1, parse_type c2)
| c1 :: cs -> FunctionT (parse_type c1, parse_function_type cs)
| _ -> raise (Failure "malformed function type")