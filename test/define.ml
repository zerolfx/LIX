open OUnit2
open Common

let tests = [
  "simple define">::(
    assert_eval_int
    2
    "(def x (: Int 2))
     x"
  );
  "define lambda">::(
    assert_eval_int
    10
    "(def id (: (-> Int Int) (fn x x)))
     (id 10)"
  );
]