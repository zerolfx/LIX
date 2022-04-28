open OUnit2

let code_to_int s = 
  let code = Lix.Parser.code_eof Lix.Lexer.token (Lexing.from_string s) in
  let ast = Lix.Pipeline.code_to_last code in
  match Lix.Jit.codegen_repl ast with
  | _, Some (Lix.Type.Int i) -> i
  | _ -> raise (Failure "result is not an int")

let assert_eval_result i s = assert_equal (code_to_int s) i


let () =
  Lix.Jit.init_jit ();
  "lambda">:::Lambda.tests |> run_test_tt_main;
  "define">:::Define.tests |> run_test_tt_main;
  "bool">:::Bool.tests |> run_test_tt_main;
  "if">:::If.tests |> run_test_tt_main;
  "general">:::General.tests |> run_test_tt_main;
  "infer">:::Infer.tests |> run_test_tt_main;
  "let">:::Let.tests |> run_test_tt_main;
