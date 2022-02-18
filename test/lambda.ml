open OUnit2
open Common

let tests = [
  "simple_add">::(
    assert_eval_int
    2
    "(+ 1 1)"
  );
  "partial application">::(
    assert_eval_int
    5
    "((+ 2) 3)"
  );
  "lambda desugar">::(
    assert_eval_int
    9
    "((: (-> Int Int Int) (fn (a b) (+ a b))) 4 5)"
  );
  "partial lambda">::(
    assert_eval_int
    6
    "((: (-> (-> Int Int) Int Int) (fn (f a) (f a))) (+ 2) 4)"
  );
  "lambda return lambda">::(
    assert_eval_int
    5
    "(((: (-> Int (-> Int Int)) 
          (fn x (: (-> Int Int)
                   (fn y (+ x y))))) 2) 3)"
  );
]