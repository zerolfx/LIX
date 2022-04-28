open OUnit2
open Common

let tests = [
  "let three args">::(
    assert_eval_int
    19
    "(let ((a 3) (b (+ 1 a)) (c (* a b))) (+ (+ a b) c))"
  );
  "let no args">::(
    assert_eval_int
    1
    "(let () 1)"
  );
  "apply">::(
    assert_eval_int
    3
    "(let ((f (+ 1)) (x 2)) (f x))"
  );
  "id">::(
    assert_eval_all
    [Lix.Type.Int 3; Lix.Type.Bool false]
    "(let ((x 3)) x)
    (let ((x false)) x)"
  );
  "compose">::(
    assert_eval_int
    5
    "(let ((f (+ 2)) (g (* 3))) (f (g 1)))"
  );
  "let lambda">::(
    assert_eval_int
    3
    "(let ((f (fn (x) (+ x 1)))) (f 2))"
  );
  "let overwrite">::(
    assert_eval_int
    2
    "(let ((x 1) (x 2)) x)"
  )
]
