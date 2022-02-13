type code =
| IntLit of int
| BoolLit of bool
| Symbol of string
| Form of code list

let rec string_of_code (c: code) = match c with
| IntLit i -> string_of_int i
| BoolLit b -> string_of_bool b
| Symbol s -> s
| Form cl ->
  "(" ^ String.concat " " (List.map string_of_code cl) ^ ")"

let print_code (c: code) = print_string ((string_of_code c) ^ "\n")