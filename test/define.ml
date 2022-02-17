open OUnit2

let rec list_last_element = function
| [] -> raise (Failure "list_last_element: empty list")
| [x] -> x
| _ :: xs -> list_last_element xs

let code_to_int s = 
  let code_list = Lix.Parser.program Lix.Lexer.token (Lexing.from_string s) in
  let ast_list = List.map Lix.Pipeline.code_to_last code_list in
  let results = List.map Lix.Jit.codegen_repl ast_list in
  match list_last_element results with
  | _, Some (Lix.Ast.Int i) -> i
  | _ -> raise (Failure "result is not an int")

let assert_eval_result i s = assert_equal (code_to_int s) i


let tests = [
  "simple define">::(fun _ -> 
    assert_eval_result
    2
    "(def x (: Int 2))
     x"
  );
  "define lambda">::(fun _ -> 
    assert_eval_result
    10
    "(def id (: (-> Int Int) (fn x x)))
     (id 10)"
  );
]