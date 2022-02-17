open OUnit2

let code_to_int s = 
  let code = Lix.Parser.code_eof Lix.Lexer.token (Lexing.from_string s) in
  let ast = Lix.Pipeline.code_to_last code in
  match Lix.Jit.codegen_repl ast with
  | _, Some (Lix.Ast.Int i) -> i
  | _ -> raise (Failure "result is not an int")

let assert_eval_result i s = assert_equal (code_to_int s) i


let tests = [
  "simple_add">::(fun _ -> 
    assert_eval_result
    2
    "(+ 1 1)"
  );
  "partial application">::(fun _ -> 
    assert_eval_result
    5
    "((+ 2) 3)"
  );
  "lambda desugar">::(fun _ -> 
    assert_eval_result
    9
    "((: (-> Int Int Int) (fn (a b) (+ a b))) 4 5)"
  );
  "partial lambda">::(fun _ -> assert_eval_result
    6
    "((: (-> (-> Int Int) Int Int) (fn (f a) (f a))) (+ 2) 4)"
  );
  "lambda return lambda">::(fun _ -> assert_eval_result
    5
    "(((: (-> Int (-> Int Int)) 
          (fn x (: (-> Int Int)
                   (fn y (+ x y))))) 2) 3)"
  );
]