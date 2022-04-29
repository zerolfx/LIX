open OUnit2
open Common

let tests = [
  "fn with identical arg names">::(fun _ ->
    assert_raises (Lix.Dast.DuplicatedArgNames "a")
    (fun _ -> eval "(fn (a b a) (+ a (+ b a)))")
  );
  "redefine global variable">::(fun _ ->
    assert_raises (Lix.Compiler.RedefinedGlobalVariable "a")
    (fun _ -> eval "(def a 10) (def a 5)")
  );
]
