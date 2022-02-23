type code =
| IntLit of int
| BoolLit of bool
| Symbol of string
| Form of code list
[@@deriving show]
