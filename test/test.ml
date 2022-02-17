open OUnit2

let code_to_int s = 
  let code = Lix.Parser.code_eof Lix.Lexer.token (Lexing.from_string s) in
  let ast = Lix.Pipeline.code_to_last code in
  match Lix.Jit.codegen_repl ast with
  | _, Some (Lix.Ast.Int i) -> i
  | _ -> raise (Failure "result is not an int")

let assert_eval_result i s = assert_equal (code_to_int s) i


let suite = 
  "lambda">:::Lambda.tests



let () =
  Lix.Jit.init_jit ();
  run_test_tt_main suite