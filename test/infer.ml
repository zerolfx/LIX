open OUnit2
open Common

let tests = [
  "basic">::(
    assert_eval_int
    3
    "((fn (x y) (+ x y)) 1 2)"
  );
  "basic 2">::(
    assert_eval_int
    3
    "((fn (f x) (f x)) (+ 1) 2)"
  );
]