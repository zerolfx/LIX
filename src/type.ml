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

exception Err of string

let rec parse_type (c: S.code): t = match c with
| S.Symbol "Int" -> IntT
| S.Symbol "Bool" -> BoolT
| S.Form [S.Symbol "->"; t1; t2] -> FunctionT (parse_type t1, parse_type t2)
| _ -> raise (Err "parse_type")