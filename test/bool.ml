open OUnit2

let code_to_bool s = 
  let code = Lix.Parser.code_eof Lix.Lexer.token (Lexing.from_string s) in
  let ast = Lix.Pipeline.code_to_last code in
  match Lix.Jit.codegen_repl ast with
  | _, Some (Lix.Ast.Bool b) -> b
  | _ -> raise (Failure "result is not an int")

let assert_eval_result b s = assert_equal (code_to_bool s) b


let tests = [
  "simple compare">::(fun _ -> 
    assert_eval_result
    true
    "(< 1 2)"
  );
  "equality">::(fun _ -> 
    assert_eval_result
    false
    "(== 2 3)"
  );
  "bool and">::(fun _ -> 
    assert_eval_result
    true
    "(&& true true)"
  );
  "bool or">::(fun _ -> assert_eval_result
    true
    "(|| true false)"
  );
  "bool not">::(fun _ -> assert_eval_result
    false
    "(not true)"
  );
]