type code =
| IntLit of int
| BoolLit of bool
| Symbol of string
| Form of code list
| CForm of string * code list
[@@deriving show]
