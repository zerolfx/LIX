open OUnit2

let code_to_int s = 
  let code = Lix.Parser.code_eof Lix.Lexer.token (Lexing.from_string s) in
  let ast = Lix.Pipeline.code_to_last code in
  match Lix.Jit.codegen_repl ast with
  | _, Some (Lix.Ast.Int i) -> i
  | _ -> raise (Failure "result is not an int")

let assert_eval_result i s = assert_equal (code_to_int s) i


let suite = 
  "lambda">:::[
    "simple_add">::(fun _ -> 
      assert_eval_result
      2
      "(+ 1 1)"
    );
    "partial_application">::(fun _ -> 
      assert_eval_result
      5
      "((+ 2) 3)"
    );
    "lambda_desugar">::(fun _ -> 
      assert_eval_result
      9
      "((: (-> Int Int Int) (fn (a b) (+ a b))) 4 5)"
    );
    "partial_lambda">::(fun _ -> assert_eval_result
      6
      "((: (-> (-> Int Int) Int Int) (fn (f a) (f a))) (+ 2) 4)"
    );
  ]



let () =
  Lix.Jit.init_jit ();
  run_test_tt_main suite