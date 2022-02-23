open OUnit2
open Common

let tests = [
  "if">::(
    assert_eval_all
    [Lix.Type.Int 1; Lix.Type.Int 2]
    "(if true 1 2) (if false 1 2)"
  );
  "nest if">::(
    assert_eval_int
    2
    "(if true (if false 1 2) 3)"
  )
]